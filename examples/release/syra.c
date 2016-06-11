int main() {
	int n=rand(10,1000);
	n = n; // Skip, pour bien voir ce qu'il se passe.
	while(n > 1) {
		if(n % 2 == 0) {
			n /= 2;
		}
		else {
			n = 3*n+1;
		}
	}
	assert(n == 1);
}
