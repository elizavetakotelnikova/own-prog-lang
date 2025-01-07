#include <unordered_set>
#include <vector>
#include <memory>
#include <iostream>
#include <algorithm>
#include "include/GCManager.h"

void GCManager::addObject(GCObject* obj){
    obj->marked = false;
    objects.insert(obj);
}

void GCManager::addRoot(GCObject* root){
    roots.push_back(root);
}

void GCManager::removeRoot(GCObject* root){
    roots.erase(
      std::remove(roots.begin(), roots.end(), root),
      roots.end()
    );
}

void GCManager::collectGarbage() {
    mark();
    sweep();
}

void GCManager::mark() {
    for (auto root : roots) {
        markObject(root);
    }
}

void GCManager::sweep() {
    
    for (auto it = objects.begin(); it != objects.end();) {
        
        if (!(*it)->marked) {
            
            delete *it;
            
            it = objects.erase(it);
            
        } else {
            
            (*it)->marked = false;
            ++it;
        }
    }
    
}

void GCManager::markObject(GCObject* obj) {
    
    if (!obj || obj->marked) return;

    obj->marked = true;

    obj->traceReferences([this](GCObject* child) { markObject(child); });
}
