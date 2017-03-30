#include "erl_nif.h"
#include "stdio.h"

static void rdiv(int r, int n, int* rem, int* ack)
{
  *rem = 0;
  *ack = 0;
  while (r > 0)
  {
    if (*rem < n)
    {
      *rem = 10 * (*rem) + 1;
      if (*rem < n)
      {
        *ack *= 10;
      }
      --r;
    }
    else
    {
      *ack = 10 * (*ack) + (*rem)/n;
      *rem %= n;
    }
  }
  if (*rem < n)
  {
    *rem %= n;
  }
  else
  {
    *ack = 10*(*ack) + (*rem)/n;
    *rem %= n;
  }
}

static ERL_NIF_TERM rdiv_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int r;
  int n;
  if (!enif_get_int(env, argv[0], &r))
  {
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[1], &n))
  {
    return enif_make_badarg(env);
  }
  int rem = 0;
  int ack = 0;
  rdiv(r, n, &rem, &ack);
  return enif_make_tuple2(env, enif_make_int(env,ack), enif_make_int(env,rem));
}

static int a(n)
{
  int i = 0;
  for (i = 1; i <= n; ++i)
  {
    int rem = 0;
    int ack = 0;
    rdiv(i, n, &rem, &ack);
    if (rem == 0)
    {
      break;
    }
  }
  return i;
}

static ERL_NIF_TERM a_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int n;
  if (!enif_get_int(env, argv[0], &n))
  {
    return enif_make_badarg(env);
  }
  return enif_make_int(env, a(n));
}

static int find(int n)
{
  int i = n % 2 == 0 ? n+1 : n;
  for (;;)
  {
    if (i % 5 != 0)
    {
      if (a(i) > n)
      {
        break;
      }
    }
    i += 2;
  }
  return i;
}

static ERL_NIF_TERM find_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int n;
  if (!enif_get_int(env, argv[0], &n))
  {
    return enif_make_badarg(env);
  }
  return enif_make_int(env, find(n));
}

static ErlNifFunc nif_funcs[] =
{
  {"rdiv", 2, rdiv_nif},
  {"a", 1, a_nif},
  {"find", 1, find_nif}
};

ERL_NIF_INIT(repunit1, nif_funcs, NULL, NULL, NULL, NULL)
