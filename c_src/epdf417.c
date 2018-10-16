#include "erl_nif.h"
#include "pdf417lib.h"
#include <string.h>

const unsigned int MAX_MSG_SIZE = 1100;
const unsigned int ANS_LEN = 4;
const unsigned int MAX_ATOM = 512;
const unsigned int MAX_FIELD = 512;
const char* GEN_ERROR = "default_error";

	ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom)
{
	ERL_NIF_TERM ret;

	if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
	{
		return enif_make_atom(env, atom);
	}

	return ret;
}

	ERL_NIF_TERM
mk_error(ErlNifEnv* env, const char* mesg)
{
	return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

	static ERL_NIF_TERM
raw_code(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	unsigned int length;
	char * msg = NULL;
	const char * err_str = GEN_ERROR;
	ERL_NIF_TERM *ans = NULL;
	ErlNifBinary bin;
	ERL_NIF_TERM res;
	ERL_NIF_TERM t;
	int isb = 0;
	int cols = 0;
	int rows = 0;
	pdf417param p;

	if (enif_inspect_binary(env, argv[0], &bin)) {
		length = bin.size;
		isb    = 1;
	}
	else {
		if (!enif_get_list_length(env, argv[0], &length)) {
			err_str = "string_length";
			goto DERR;
		}
	}
	if (length > MAX_MSG_SIZE ){
		err_str = "text_too_long";
		goto DERR;
	}
	if (isb){
		memcpy(msg,(const char*)bin.data,length);
		msg[length] = '\0';
	}
	else{
		if (enif_get_string(env, argv[0], msg, length+1, ERL_NIF_LATIN1) < 1) {
			err_str = "getting_string";
			goto DERR;
		}
	}
	pdf417init(&p);
	p.text = msg;
	p.options = PDF417_INVERT_BITMAP;
	paintCode(&p);
	if (p.error){
		pdf417free(&p);
		goto DERR;
	}
	ErlNifBinary raw_bin;
	enif_alloc_binary(p.lenBits, &raw_bin);
	memcpy(raw_bin.data, p.outBits, p.lenBits);

	ans = (ERL_NIF_TERM*)enif_alloc(ANS_LEN*sizeof(ERL_NIF_TERM));

	t = enif_make_binary (env,raw_bin);
	ans[0] =  enif_make_tuple2 (env,mk_atom(env,"raw_data"), t);
	t = enif_make_int (env, p.lenBits);
	ans[1] =  enif_make_tuple2 (env,mk_atom(env,"len_data"), t);

	cols = p.bitColumns / 8 + 1;
	t = enif_make_int (env, cols);
	ans[2] =  enif_make_tuple2 (env,mk_atom(env,"len_columns"), t);

	rows = p.codeRows;
	t = enif_make_int (env, rows);
	ans[3] =  enif_make_tuple2 (env,mk_atom(env,"len_rows"), t);

	pdf417free(&p);
	free(msg);
	enif_free(ans);

	return enif_make_tuple2 (env,mk_atom(env, "ok"),res);

DERR:
	res = mk_error(env, err_str);
	if (msg){
		free(msg);
	}
	return res;

}

static ErlNifFunc nif_funcs[] = {
	{"raw_code", 1, raw_code}
};

ERL_NIF_INIT(epdf417, nif_funcs, NULL, NULL, NULL, NULL);
