#include <unordered_set>
#include <vector>
#include <functional>

class GCObject {
public:
    bool marked = false;
    virtual ~GCObject() = default;
    virtual void traceReferences(std::function<void(GCObject*)> visitor) = 0;
};

class GCManager {
private:
    std::unordered_set<GCObject*> objects;
    std::vector<GCObject*> roots;
    void mark();
    void markObject(GCObject* obj);
    void sweep();

public:
    void addObject(GCObject* obj);
    void addRoot(GCObject* root);
    void removeRoot(GCObject* root);
    void collectGarbage();
};