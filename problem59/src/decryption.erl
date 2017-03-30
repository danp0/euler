-module(decryption).

-export(
   [
    main/0,
    read_cipher/1,
    xor_binary/2
   ]
  ).

-define(Cipher, "p059_cipher.txt").

main() ->
  Cipher = read_cipher(?Cipher),
  Head = binary:part(Cipher, {0, 80}),
  Lowercase = lists:seq($a, $z),
  {ok, The} = re:compile(" [Tt][Hh][Ee] "),
  Candidates =
  lists:filter(
    fun(Key) ->
        Xor = xor_binary(Key, Head),
        re:run(binary_to_list(Xor), The, [{capture, none}]) =:= match
    end,
    [<< A, B, C >> || A <- Lowercase, B <- Lowercase, C <- Lowercase]),
  lists:foreach(
    fun(Key) ->
        String = binary_to_list(xor_binary(Key, Cipher)),
        io:format("~p:~n~p~n~p~n", [Key, String, lists:sum(String)])
    end,
    Candidates),
  ok.

read_cipher(Filename) ->
  Temp = Filename ++ ".tmp",
  {ok, Binary} = file:read_file(Filename),
  file:write_file(Temp, << <<"<<">>/binary, Binary/binary, <<">>.">>/binary>>),
  {ok, Terms} = file:consult(Temp),
  hd(Terms).

xor_binary(<<A1:8,A2:8,A3:8>> = Key, Text) ->
  case byte_size(Text) rem 3 of
    2 ->
      X1 = binary:part(Text, {0, byte_size(Text) - 2}),
      <<X2, X3>> = binary:part(Text, {byte_size(Text), - 2}),
      XOR1 = xor_binary(Key, X1),
      << XOR1/binary, (X2 bxor A1), (X3 bxor A2) >>;
    1 ->
      Y1 = binary:part(Text, {0, byte_size(Text) - 1}),
      <<Y2>> = binary:part(Text, {byte_size(Text), - 1}),
      XOR2 = xor_binary(Key, Y1),
      << XOR2/binary, (Y2 bxor A1) >>;
    0 ->
      << <<(B1 bxor A1), (B2 bxor A2), (B3 bxor A3)/integer>> ||
         <<B1:8, B2:8, B3:8>> <= Text >>
  end.

%%
%% unit test
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

xor_binary_test_() ->
  [
   ?_assertEqual(
      <<"123456">>, 
      xor_binary(<<"abc">>, xor_binary(<<"abc">>, <<"123456">>))),
   ?_assertEqual(
      <<"1234567">>, 
      xor_binary(<<"abc">>, xor_binary(<<"abc">>, <<"1234567">>))),
   ?_assertEqual(
      <<"12345678">>, 
      xor_binary(<<"abc">>, xor_binary(<<"abc">>, <<"12345678">>)))
  ].

-endif.
