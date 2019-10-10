%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

Nonterminals segments segment subrects slices slice int.
Terminals name integer '{' '}' '[' ']' ':' ','.
Rootsymbol subrects.

subrects -> subrects ',' segments:
  '$1' ++ ['$3'].

subrects -> segments:
  ['$1'].

segments -> segment segments:
  {'$1', ['$2']}.

segments -> segment '{' subrects '}':
  {'$1', '$3'}.

segments -> segment:
  '$1'.

segment -> name:
  field('$1').

segment -> '[' ']':
  [].

segment -> '[' slices ']':
  '$2'.

slices -> slices ',' slice:
  '$1' ++ ['$3'].

slices -> slice:
  ['$1'].

slice -> int:
  '$1'.

slice -> int ':' int:
  {'$1', 1, '$3'}.

slice -> int ':':
  {'$1', 1}.

slice -> ':' int:
  {0, 1, '$2'}.

slice -> int ':' int ':' int:
  {'$1', '$3', '$5'}.

slice -> int ':' int ':' :
  {'$1', '$3'}.

slice -> ':' int ':' int:
  {0, '$2', '$4'}.

int -> integer:
  int('$1').

Erlang code.

field({name, _, Name}) ->
    list_to_binary(Name).

int({integer, _, Int}) ->
    Int.
