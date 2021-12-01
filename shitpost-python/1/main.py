from __future__ import annotations
from collections.abc import Sequence, Callable, Iterable
from typing import TypeVar, Generic
from itertools import pairwise, starmap
import operator


T = TypeVar("T")
T2 = TypeVar("T2")
O = TypeVar("O")


def tupled(f: Callable[[T, T2], O]) -> Callable[[Tuple[T, T2]], O]:
    return lambda t: f(t[0], t[1])


class Stream(Generic[T]):
    underlying: Iterable[T]

    def __init__(self, input: Iterable[T]) -> None:
        self.underlying = input

    def map(self, f: Callable[[T], T2]) -> Stream[T2]:
        return Stream(map(f, self.underlying))

    def toIterator(self) -> Iterable[T]:
        return self.underlying

    def zipWithNext(self) -> Stream[Tuple[T, T]]:
        return Stream(pairwise(self.underlying))

    def runCount(self, predicate: Callable[[T], bool]) -> int:
        return sum(map(predicate, self.underlying))

    def runToList(self) -> List[T]:
        return list(self.underlying)


def identity(t: T) -> T:
    return t


if __name__ == "__main__":
    with open("input.txt") as fh:
        r = (
            Stream(fh)
            .map(int)
            .zipWithNext()
            .runCount(tupled(operator.lt))
        )
        print(r)
