#include "kvstore.h"

void kvstore_prog_1(int key, char *value) {
	CLIENT *clnt = clnt_create ("localhost", KVSTORE_PROG, KVSTORE_VERS, "tcp");
	if (clnt == NULL) {
		clnt_pcreateerror ("localhost");
		exit (1);
	}
	
	kvpair post_1_arg = {key, value};
	void * res = post_1(&post_1_arg, clnt);
	if (res == NULL) {
		clnt_perror (clnt, "call failed");
	}
	clnt_destroy (clnt);
}


int main (int argc, char *argv[]) {
	char *host;

	if (argc < 3) {
		printf ("usage: %s <key> <value>\n", argv[0]);
		exit (1);
	}
	int key = atoi(argv[1]);
	char *value = argv[2];
	kvstore_prog_1(key, value);
	return 0;
}
