/*
 *  C stub to be plugged into OCaml
 */

#include <caml/mlvalues.h>

#include <stdio.h>

CAMLprim value
hello_world (value n)
{
	printf("hello world %i!\n", Int_val(n));
	return Val_unit;
}

#include <sys/socket.h>
#include <netinet/in.h>

CAMLprim value
setsockopt_dontfrag (value sock)
{
	/*	http://stackoverflow.com/questions/973439 */
#ifdef IP_DONTFRAG
	int val = 1;
	int r = setsockopt(Int_val(sock), IPPROTO_IP, IP_DONTFRAG, &val, sizeof(val));
#else
	int val = IP_PMTUDISC_DO;
	int r = setsockopt(Int_val(sock), IPPROTO_IP, IP_MTU_DISCOVER, &val, sizeof(val));
#endif
	return Val_bool(r != -1);
	/* errors should be handled more properly (raising an exception with errno) */
}

#include <errno.h>

CAMLprim value
get_errno (value unit)
{
	return Val_int(errno);
}
