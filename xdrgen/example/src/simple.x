enum Things { A, B, C };
struct Bar {
       opaque data<>;
};
struct Foo {
	int a;
	int b;
	int c;
	Bar bar<>;
	Bar *barish;
	string name<>;
	Things thing;
	unsigned type;
};
