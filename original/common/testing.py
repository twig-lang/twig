ALL_TESTS = []


def test(function):
    index = len(ALL_TESTS)

    function.test_name = function.__module__ + "." + function.__name__
    function.test_description = function.__annotations__.get("description") or None
    function.test_should_fail = function.__annotations__.get("should_fail") or False
    function.test_todo = function.__annotations__.get("todo") or False

    ALL_TESTS.append(function)

    return function


def description(name):
    def deco(fn):
        fn.__annotations__["description"] = name
        return fn

    return deco


def should_fail(fn):
    fn.__annotations__["should_fail"] = True
    return fn


def todo(reason=None):
    def deco(fn):
        fn.__annotations__["todo"] = True
        fn.test_todo_reason = reason
        return fn

    return deco


@test
@description("this test always passes")
def always_passes():
    pass


@test
@description("this test never passes")
@should_fail
def always_fails():
    raise Exception("failing!")


@test
@description("not finished yet")
@should_fail
@todo("left to do later")
def to_do():
    pass


@test
@description("will skip")
def skips():
    skip(reason="there is a reason...")


class _Skip(Exception):
    def __init__(self, reason=None):
        self.reason = reason


def skip(reason=None):
    raise _Skip(reason)


def run():
    print(f"1..{len(ALL_TESTS)}")

    for index, test in enumerate(ALL_TESTS):
        passes = True
        skipped = False
        todo = test.test_todo
        reason = None
        description = test.test_description

        try:
            test()
        except _Skip as s:
            skipped = True
            reason = s.reason
        except:
            passes = False

        if test.test_should_fail:
            passes = not passes

        if not passes:
            print("not ", end="")

        print(f"ok {index}", end="")

        if description:
            print(f" - {description}", end="")

        if skipped:
            print(" # SKIP", end="")

        elif todo:
            print(" # TODO", end="")
            reason = test.test_todo_reason

        if skipped or todo:
            if reason is not None:
                print(f" {reason}", end="")

        print()
    pass
