from __future__ import annotations
from collections.abc import Sequence, Callable, Iterable
from collections import deque
from typing import TypeVar, Generic
from itertools import pairwise, starmap, islice
import operator


T = TypeVar("T")
T2 = TypeVar("T2")
O = TypeVar("O")


def tupled(f: Callable[[T, T2], O]) -> Callable[[Tuple[T, T2]], O]:
    return lambda t: f(t[0], t[1])


def slider(input: Iterable[T], window: int) -> Iterable[list[T]]:
    i = iter(input)
    queue = deque(islice(i, window - 1), window)
    for item in i:
        queue.append(item)
        yield list(queue)


def _tap(f: Callable[T, T2], x: T) -> T:
    f(x)
    return x


class Stream(Generic[T]):
    underlying: Iterable[T]

    def __init__(self, input: Iterable[T]) -> None:
        self.underlying = input

    def map(self, f: Callable[[T], T2]) -> Stream[T2]:
        return Stream(map(f, self.underlying))

    def tap(self, f: Callable[[T], T2]) -> Stream[T]:
        return Stream(map(lambda x: _tap(f, x), self.underlying))

    def toIterator(self) -> Iterable[T]:
        return self.underlying

    def zipWithNext(self) -> Stream[Tuple[T, T]]:
        return Stream(pairwise(self.underlying))

    def runCount(self, predicate: Callable[[T], bool]) -> int:
        return sum(map(predicate, self.underlying))

    def runToList(self) -> List[T]:
        return list(self.underlying)

    # methods for part 2

    def sliding(self, window: int) -> Stream[list[t]]:
        return Stream(slider(self.underlying, window))


def identity(t: T) -> T:
    return t


if __name__ == "__main__":
    with open("input.txt") as fh:
        print("part 1")
        r = Stream(fh).map(int).zipWithNext().runCount(tupled(operator.lt))
        print(r)

    with open("input.txt") as fh:
        print("part 2")
        r = (
            Stream(fh)
            .map(int)
            .sliding(3)
            .map(sum)
            .zipWithNext()
            .runCount(tupled(operator.lt))
        )
        print(r)
