int main() {
	int x = rand(0,1000);
	int y = -x;
	int s = x+y+1;
	int z = 42/s; // = 42
	assert(z == 42);
}

