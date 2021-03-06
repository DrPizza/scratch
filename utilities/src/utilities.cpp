#include "stdafx.h"

#include "tuple-utils.hpp"

int test() {
	constexpr auto t = std::make_tuple(42, 'z', 3.14, 13, 0, "Hello, World!");
	constexpr auto s = std::make_tuple("Hello, World!", 0, 13, 3.14, 'z', 42);

	const auto r = tuple_visit([](const auto& x) {
		std::cout << x << std::endl;
		return x;
	}, t);

	static_assert(std::is_same_v<decltype(t), decltype(r)>);

	tuple_visit([](const auto&... xs) {
		tuple_visit([](const auto& x) {
			std::cout << x << " ";
		}, std::make_tuple(xs...));
		std::cout << std::endl;
	}, t, s);

	constexpr auto u = tuple_visit([](const auto&... xs) {
		return std::make_tuple(xs...);
	}, t, s);

	std::cout << typeid(u).name() << std::endl;

	return 0;
}

int main() {
	return 0;
}
