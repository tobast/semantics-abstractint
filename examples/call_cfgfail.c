/*
 * Ce fichier échoue : le CFG pense que le 'a' multiplié par 2 est le
 * 'a' local à 'add'...
 */

int add(int a, int b) {
	return a+b;
}

void main() {
	int a = add(2,3) + add(3,4);
	a *= 2;
	assert(a >= 20 && a <= 28);
}

