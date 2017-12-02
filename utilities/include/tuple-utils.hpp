#pragma once

#include <tuple>
#include <type_traits>

namespace {
	template<typename... Ts>
	constexpr bool is_any_void_v = std::disjunction_v<std::is_void<Ts>...>;

	template<typename T, typename... Ts>
	constexpr bool are_all_same_size_v = ((std::tuple_size_v<std::decay_t<T>> == std::tuple_size_v<std::decay_t<Ts>>) && ...);

	template<std::size_t N,
	         typename Visitor,
	         typename... Tuples>
	constexpr decltype(auto) tuple_visit_aux(Visitor&& v, Tuples&&... ts) {
		return std::forward<Visitor>(v)(std::get<N>(std::forward<Tuples>(ts))...);
	}

	template<typename Visitor,
	         std::size_t... Is,
	         typename... Tuples,
	         typename = std::enable_if_t<is_any_void_v<decltype(tuple_visit_aux<Is>(std::declval<Visitor>(), std::declval<Tuples>()...))...> > >
	constexpr void tuple_visit_aux(Visitor&& v, std::index_sequence<Is...>, Tuples&&... ts) {
		static_cast<void>(
			(tuple_visit_aux<Is>(std::forward<Visitor>(v), std::forward<Tuples>(ts)...), ...)
		);
	}

	template<typename Visitor,
	                  std::size_t... Is,
	                  typename... Tuples,
	                  typename = std::enable_if_t<!is_any_void_v<decltype(tuple_visit_aux<Is>(std::declval<Visitor>(), std::declval<Tuples>()...))...> > >
	constexpr decltype(auto) tuple_visit_aux(Visitor&& v, std::index_sequence<Is...>, Tuples&&... ts) {
		return std::make_tuple(tuple_visit_aux<Is>(std::forward<Visitor>(v), std::forward<Tuples>(ts)...)...);
	}
}

template<typename Visitor, typename Tuple, typename... Tuples, typename = std::enable_if_t<are_all_same_size_v<Tuple, Tuples...>>>
constexpr decltype(auto) tuple_visit(Visitor&& v, Tuple&& t, Tuples&&... ts) {
	return tuple_visit_aux(v, std::make_index_sequence<std::tuple_size_v<std::decay_t<Tuple>>>{}, t, ts...);
}
