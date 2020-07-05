

#include "RelearningCpp.h"

#include <GLFW/glfw3.h>

#include <algorithm>
#include <functional>
#include <optional>
#include <tuple>
#include <vector>

#include <array>

using uint32 = uint32_t;
using int32 = int32_t;
using uint8 = uint8_t;
using usize = std::size_t;

struct TypeInfo {
    usize id;
    usize size;

    template<class T>
    static TypeInfo create() {
        return TypeInfo{ typeid(T).hash_code(), sizeof(T) };
    }

    bool operator==(const TypeInfo& other) const { return id == other.id; }

    bool operator<(const TypeInfo& other) const { return id < other.id; }
};

struct EntityHandle {
    usize formation;
    usize index;

    EntityHandle(usize _formation, usize _index)
      : formation(_formation)
      , index(_index) {}
};

/*
Basic component storage, mirroring std::vector behaviour.
Removal of entities is done through swap-and-pop.
Keeps an insert list of free slots.

Components must therefore be:
        - Trivially Copyable

*/

class VectorStorage {
    static const usize INIT_LEN = 32;

    TypeInfo typeInfo;

    usize len = 0;
    usize capacity = INIT_LEN;
    void* data = nullptr;

    VectorStorage(TypeInfo _typeInfo, void* _data)
      : typeInfo(_typeInfo)
      , data(_data) {}

  public:
    class Iterator {
      public:
        Iterator(): storage(nullptr), index(-1){}

        Iterator(const VectorStorage* _storage, usize _index)
          : storage(_storage)
          , index(_index) {}
        const void* operator*() const {
            auto ptr = storage->_get_elem_ptr(index);
            return ptr;
        }
        Iterator operator++() {
            auto oldIndex = index;
            index++;
            return Iterator(storage, oldIndex);
        }
        bool operator!=(const Iterator& other) const {
            return index != other.index;
        }

      private:
        const VectorStorage* storage;
        usize index;
    };
    friend Iterator;
    friend class World;

    static VectorStorage create(TypeInfo typeInfo) {
        void* _data = malloc(typeInfo.size * INIT_LEN);

        return { typeInfo, _data };
    }

    void insert(void* comp) { _push_back(comp); }

    void remove(usize index) {
        void* currentPtr = _get_elem_ptr(index);
        void* lastPtr = last();
        memcpy(currentPtr, lastPtr, typeInfo.size);
        len--;
    }

    void* last() { return _get_elem_ptr(len - 1); }

    Iterator begin() const { return Iterator(this, 0); }

    Iterator end() const { return Iterator(this, len); }

    usize size() const { return len; }

    TypeInfo get_type_info() const { return typeInfo; }

  private:
    /// vec impl

    void _push_back(void* data) {
        if (len == capacity) {
            _grow();
        }

        void* elemPtr = _get_elem_ptr(len);
        memcpy(elemPtr, data, typeInfo.size);

        len++;
    }

    void* _get_elem_ptr(usize offset) const {
        auto _offset = (offset * typeInfo.size);
        return (void*)((uint8*)data + _offset);
    }

    void _grow() {
        data = realloc(data, _current_block_size() * 2);
        capacity = capacity * 2;
    }

    usize _current_block_size() { return typeInfo.size * capacity; }
};

class EntityFormation {
    std::vector<TypeInfo> types;
    std::vector<VectorStorage> components;

  public:
    EntityFormation(std::vector<TypeInfo> _types)
      : types(_types) {
        std::sort(types.begin(), types.end());

        components.reserve(8);
        for (auto& t : types) {
            components.push_back(VectorStorage::create(t));
        }
    }

    usize insert(std::initializer_list<void*> new_components) {
        _ASSERT(new_components.size() == types.size());
        auto index = next_index();

        usize i = 0;
        for (auto c : new_components) {
            components[i].insert(c);
            i++;
        }

        return index;
    }

    const std::vector<TypeInfo>& get_types() const { return types; }

    std::vector<VectorStorage>& get_components() { return components; }

    usize next_index() {
        // VectorStorage's next index is always the current length
        // All component's VectorStorages must be the same length at all times
        // Therefore this is correct
        return components[0].size();
    }

  private:
};

struct Vector3 {
    float x;
    float y;
    float z;
};

struct Transform {
    Vector3 position;
};

struct Ball {
    float radius;
};

/*
        template<class T>
        class CollectTypeIds {
        public:
                static void collect_into(std::vector<TypeInfo>& output) {
                        output.push_back(TypeInfo::create<T>());
                }
        };
*/

template<class... Ts>
class CollectTypeIds;

template<class T, class... Ts>
class CollectTypeIds<T, Ts...> {
  public:
    static void collect_into(std::vector<TypeInfo>& output) {
        output.push_back(TypeInfo::create<T>());
        CollectTypeIds<Ts...>::collect_into(output);
    }
};

template<>
class CollectTypeIds<> {
  public:
    static void collect_into(std::vector<TypeInfo>& output) {}
};

template<typename... Ts>
std::array<TypeInfo, sizeof...(Ts)> get_types()
{
    return { TypeInfo::create<Ts>()... };
}


template<typename Callable, typename Iter, typename FirstArgType, typename... RestTypes>
struct DispatchWrapper;

template<typename Callable, typename Iter, typename FirstArgType, typename... RestTypes>
struct DispatchWrapper {
    template<typename... CollectedTypes>
    static void dispatch(Callable c, Iter i, CollectedTypes... collected) {
        std::cout << typeid(FirstArgType).hash_code() << "\n";

        VectorStorage::Iterator inner_iter = *i;
        FirstArgType& arg = *((FirstArgType*)(*inner_iter));
        inner_iter.operator++();
        DispatchWrapper<Callable, Iter, RestTypes...>::dispatch(c, ++i, arg, collected...);
    }
};


template<typename Callable, typename Iter, typename FirstArgType>
struct DispatchWrapper<Callable, Iter, FirstArgType> {
    template<typename... CollectedTypes>
    static void dispatch(Callable c, Iter i, CollectedTypes... collected) {
        VectorStorage::Iterator inner_iter = *i;
        FirstArgType& arg = *((FirstArgType*)(*inner_iter));
        inner_iter.operator++();
        std::cout << "hello twice";
        c(collected..., arg);
    }
};


/*
template<typename Callable, typename Iter, typename Arg,  typename... Ts, typename... Collected>
void dispatch(Callable c, Iter i, Collected... collected) {
    dispatch<Ts...>(c, *((Arg)(*i)), ++i);
}

template<typename Callable, typename Iter, typename... Collected>
void dispatch<Callable, Iter, void, void, Collected...>(Callable c, Iter i, Collected... collected) {
    c(collected...);
}
*/

class World {
    std::vector<EntityFormation> formations;

    World(std::vector<EntityFormation> _formations)
      : formations(_formations) {}

  public:
    static World empty() {
        auto emptyFormation = EntityFormation({});
        return World({ emptyFormation });
    }

    EntityHandle create_entity() { return EntityHandle(0, 0); }

    template<class T>
    EntityHandle add_component(EntityHandle entity, T component) {
        // to add a component to an entity, we need to remove it from it's
        // current formation first then we insert it into the formation matching
        // it's new structure, creating that formation if it does not exist

        EntityFormation* old_formation = &formations[entity.formation];
        auto formationIndex = find_formation_extra(old_formation->get_types(),
                                                   TypeInfo::create<T>());

        if (!formationIndex) {
            formationIndex = create_formation_extra(old_formation->get_types(),
                                                    TypeInfo::create<T>());
            old_formation = &formations[entity.formation];
        }

        EntityFormation* formation = &formations[formationIndex.value()];

        auto& from_components = old_formation->get_components();
        auto& to_components = formation->get_components();

        auto copyFrom = from_components.begin();
        auto copyTo = to_components.begin();

        auto storageIndex = formation->next_index();

        while (copyTo != to_components.end()) {
            auto& to = *copyTo;

            if (to.get_type_info() == TypeInfo::create<T>()) {
                to.insert(&component);
                copyTo++;
                continue;
            }

            auto& from = *copyFrom;
            _ASSERT(from.get_type_info() == to.get_type_info());

            to.insert(from._get_elem_ptr(entity.index));
            from.remove(entity.index);

            copyFrom++;
            copyTo++;
        }

        return EntityHandle(formationIndex.value(), storageIndex);
    }

    std::optional<usize> find_formation_extra(
      const std::vector<TypeInfo>& types,
      TypeInfo extra) {
        for (auto i = 0; i < formations.size(); i++) {
            if (formation_has_types_with_extra_2(
                  formations[i].get_types(), types, extra)) {
                return i;
            }
        }

        return std::nullopt;
    }

    bool formation_has_types_with_extra_2(
      const std::vector<TypeInfo>& formation_types,
      const std::vector<TypeInfo>& types,
      TypeInfo extra) {
        if (formation_types.size() != (types.size() + 1)) {
            return false;
        }

        auto [partition_point, extra_it] =
          std::mismatch(types.begin(), types.end(), formation_types.begin());

        if (*extra_it != extra) {
            return false;
        }

        return std::equal(
          std::next(extra_it), formation_types.end(), partition_point);
    }

    usize create_formation_extra(const std::vector<TypeInfo>& types,
                                 TypeInfo extra) {
        // bite the bullet and copy types once.
        // this should be rare enough to not matter
        std::vector<TypeInfo> new_types = types;
        new_types.push_back(extra);

        formations.emplace_back(new_types);
        return formations.size() - 1;
    }

    template<class... CompTypes>
    void with_comps(void (*cb)(CompTypes...)) {
        auto types = get_types<CompTypes...>();
        std::sort(types.begin(), types.end());

        for (auto& t : types) {
            std::cout << t.id << "\n";
        }

        for (auto& f: formations) {
            auto& f_types = f.get_types();
            auto f_iter = f_types.begin();
            auto t_iter = types.begin();
            auto f_index = 0;
            auto t_index = 0;

            std::array<VectorStorage::Iterator, sizeof...(CompTypes)> storage_pointers = {};

            while (f_iter != f_types.end()) {    
                if (*f_iter == *t_iter) {
                    storage_pointers[t_index] = f.get_components()[f_index].begin();

                    t_iter++;
                    t_index++;

                    if (t_iter == types.end()) {
                        DispatchWrapper<decltype(cb), decltype(storage_pointers.begin()), CompTypes...>::dispatch(cb, storage_pointers.begin());
                        goto next;
                    }
                }

                f_iter++;
                f_index++;
            }
            next:
                continue;
        }
    }
};

int main() {
    auto world = World::empty();

    for (int i = 0; i < 200; i++) {
        auto entity = world.create_entity();

        entity = world.add_component(entity, Transform{ Vector3{ 1.0, 5.0, 3.0 } });
        entity = world.add_component(entity, Ball{ (float)i });
    }

    for (int i = 0; i < 50; i++) {
        auto entity = world.create_entity();
        entity = world.add_component(entity, Transform{ Vector3{ 1.0, 2.0, 4.0 } });
    }

    for (int i = 0; i < 50; i++) {
        auto entity = world.create_entity();
        entity = world.add_component(entity, Ball{ (float)i });
    }

    world.with_comps<Transform, Ball>(
      +[](Transform transform, Ball ball) { 
          std::cout << "Hello world?";
          std::cout << ball.radius; });

    std::cout << std::flush;

    glfwInit();
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

    GLFWwindow* window = glfwCreateWindow(800, 600, "LearnOpenGL", NULL, NULL);
    if (window == NULL) {
        std::cout << "Failed to create GLFW window" << std::endl;
        glfwTerminate();
        return -1;
    }
    glfwMakeContextCurrent(window);

    glViewport(0, 0, 800, 600);

    auto processInput = [](GLFWwindow* window) {
        if (glfwGetKey(window, GLFW_KEY_ESCAPE) == GLFW_PRESS)
            glfwSetWindowShouldClose(window, true);
    };

    while (!glfwWindowShouldClose(window)) {
        processInput(window);

        glClearColor(0.2f, 0.3f, 0.3f, 1.0f);
        glClear(GL_COLOR_BUFFER_BIT);

        glfwPollEvents();
        glfwSwapBuffers(window);
    }

    glfwTerminate();
    return 0;
}
