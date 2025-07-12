#include<iostream>

struct A{};
struct B1 : virtual A {};
struct B2 : virtual A {};
struct C: B1, B2{};
struct D: virtual C, B2 {};