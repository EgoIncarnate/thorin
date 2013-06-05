#include "anydsl2/transform/lower2cff.h"

#include <iostream>
#include <boost/unordered_map.hpp>

#include "anydsl2/lambda.h"
#include "anydsl2/world.h"
#include "anydsl2/type.h"
#include "anydsl2/analyses/scope.h"
#include "anydsl2/analyses/verifier.h"
#include "anydsl2/transform/mangle.h"
#include "anydsl2/transform/merge_lambdas.h"

namespace anydsl2 {

class CFFLowering {
public:
    CFFLowering(World& world)
        : world(world)
    {
        Scope scope(world);
        top.insert(scope.entries().begin(), scope.entries().end());
    }

    void transform(Lambda* lambda);
    size_t process();

private:

    World& world;
    LambdaSet top;
};

void CFFLowering::transform(Lambda* lambda) {
    Scope scope(lambda);
    typedef boost::unordered_map<Array<const Def*>, Lambda*> Args2Lambda;
    Args2Lambda args2lambda;

    for_all (use, lambda->copy_uses()) {
        if (use.index() != 0 || !use->isa<Lambda>())
            continue;

        Lambda* ulambda = use->as_lambda();
        if (scope.contains(ulambda))
            continue;

        GenericMap generic_map;
        bool res = lambda->type()->infer_with(generic_map, ulambda->arg_pi());
        assert(res);
        
        size_t size = lambda->num_params();
        Array<size_t> indices(size);
        Array<const Def*> with(size);
        Array<const Def*> args(size);

        size_t num = 0;
        for (size_t i = 0; i != size; ++i) {
            if (lambda->param(i)->order() >= 1) {
                const Def* arg = ulambda->arg(i);
                indices[num] = i;
                with[num++] = arg;
                args[i] = arg;
            } else
                args[i] = 0;
        }
        with.shrink(num);
        indices.shrink(num);

        // check whether we can reuse an existing version
        Args2Lambda::iterator args_i = args2lambda.find(args);
        Lambda* target;
        if (args_i != args2lambda.end()) 
            target = args_i->second; // use already dropped version as jump target 
        else
            args2lambda[args] = target = drop(scope, indices, with, generic_map);

        ulambda->jump(target, ulambda->args().cut(indices));
    }
}

size_t CFFLowering::process() {
    std::vector<Lambda*> todo;
    for_all (top_lambda, top) {
        Scope scope(top_lambda);
        for (size_t i = scope.size(); i-- != 0;) {
            Lambda* lambda = scope[i];
            if (lambda->num_params()                            // is there sth to drop?
                && (lambda->is_generic()                        // drop generic stuff
                    || (!lambda->is_basicblock()                // don't drop basic blocks
                        && (!lambda->is_returning()             // drop non-returning lambdas
                            || top.find(lambda) == top.end()))))// lift/drop returning non top-level lambdas
                todo.push_back(lambda);
        }
    }

    for_all (lambda, todo)
        transform(lambda);

    return todo.size();
}

void lower2cff(World& world) {
    size_t todo;
    do {
        CFFLowering lowering(world);
        todo = lowering.process();
        assert(verify(world) && "invalid cfg transform");
        merge_lambdas(world);
        assert(verify(world) && "invalid merge lambda transform");
        world.cleanup();
        assert(verify(world) && "after cleanup");
    } while (todo);
    merge_lambdas(world);
    world.cleanup();
}

} // namespace anydsl2