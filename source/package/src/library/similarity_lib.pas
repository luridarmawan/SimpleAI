{
This file is part of the SimpleAI package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit similarity_lib;

{$mode objfpc}{$H+}

interface

uses
  Math,
  Classes, SysUtils;


// https://wiki.freepascal.org/Levenshtein_distance
function LevenshteinDistance(const s1, s2: string): integer;
function LevenshteinDistanceText(const s1, s2: string): integer;

// https://stackoverflow.com/questions/54797/how-do-you-implement-levenshtein-distance-in-delphi
function EditDistance(const s, t: string): integer;

function JaroWinkler(const s1, s2: string): double;

implementation

{------------------------------------------------------------------------------
  Name:    LevenshteinDistance
  Params: s1, s2 - strings
  Returns: Minimum number of single-character edits.
  Compare 2 strings, case sensitive.
------------------------------------------------------------------------------}
function LevenshteinDistance(const s1, s2: string): integer;
var
  length1, length2, i, j, value1, value2, value3: integer;
  matrix: array of array of integer;
begin
  length1 := Length(s1);
  length2 := Length(s2);
  SetLength(matrix, length1 + 1, length2 + 1);
  for i := 0 to length1 do
    matrix[i, 0] := i;
  for j := 0 to length2 do
    matrix[0, j] := j;
  for i := 1 to length1 do
    for j := 1 to length2 do
    begin
      if Copy(s1, i, 1) = Copy(s2, j, 1) then
        matrix[i, j] := matrix[i - 1, j - 1]
      else
      begin
        value1 := matrix[i - 1, j] + 1;
        value2 := matrix[i, j - 1] + 1;
        value3 := matrix[i - 1, j - 1] + 1;
        matrix[i, j] := min(value1, min(value2, value3));
      end;
    end;
  Result := matrix[length1, length2];
end;

{------------------------------------------------------------------------------
  Name:    LevenshteinDistanceText
  Params: s1, s2 - strings
  Returns: Minimum number of single-character edits.
  Compare 2 strings, case insensitive.
------------------------------------------------------------------------------}
function LevenshteinDistanceText(const s1, s2: string): integer;
var
  s1lower, s2lower: string;
begin
  s1lower := LowerCase(s1);
  s2lower := LowerCase(s2);
  Result := LevenshteinDistance(s1lower, s2lower);
end;

function EditDistance(const s, t: string): integer;
var
  d: array of array of integer;
  i, j, cost: integer;
begin
  {
  Compute the edit-distance between two strings.
  Algorithm and description may be found at either of these two links:
  http://en.wikipedia.org/wiki/Levenshtein_distance
  http://www.google.com/search?q=Levenshtein+distance
  }

  //initialize our cost array
  SetLength(d, Length(s) + 1);
  for i := Low(d) to High(d) do
  begin
    SetLength(d[i], Length(t) + 1);
  end;

  for i := Low(d) to High(d) do
  begin
    d[i, 0] := i;
    for j := Low(d[i]) to High(d[i]) do
    begin
      d[0, j] := j;
    end;
  end;

  //store our costs in a 2-d grid
  for i := Low(d) + 1 to High(d) do
  begin
    for j := Low(d[i]) + 1 to High(d[i]) do
    begin
      if s[i] = t[j] then
      begin
        cost := 0;
      end
      else
      begin
        cost := 1;
      end;

      //to use "Min", add "Math" to your uses clause!
      d[i, j] := Min(Min(d[i - 1, j] + 1,      //deletion
        d[i, j - 1] + 1),     //insertion
        d[i - 1, j - 1] + cost  //substitution
        );
    end;  //for j
  end;  //for i

  //now that we've stored the costs, return the final one
  Result := d[Length(s), Length(t)];

  //dynamic arrays are reference counted.
  //no need to deallocate them
end;

function JaroWinkler(const s1, s2: string): double;
var
  l1, l2, match_distance, matches, i, k, trans: integer;
  bs1, bs2: array[1..255] of boolean; //used to avoid getmem, max string length is 255
begin
  l1 := length(s1);
  l2 := length(s2);
  fillchar(bs1, sizeof(bs1), 0); //set booleans to false
  fillchar(bs2, sizeof(bs2), 0);
  if l1 = 0 then
    if l2 = 0 then
      exit(1)
    else
      exit(0);
  match_distance := (max(l1, l2) div 2) - 1;
  matches := 0;
  trans := 0;
  for i := 1 to l1 do
  begin
    for k := max(1, i - match_distance) to min(i + match_distance, l2) do
    begin
      if bs2[k] then
        continue;
      if s1[i] <> s2[k] then
        continue;
      bs1[i] := True;
      bs2[k] := True;
      Inc(matches);
      break;
    end;
  end;
  if matches = 0 then
    exit(0);
  k := 1;
  for i := 1 to l1 do
  begin
    if (bs1[i] = False) then
      continue;
    while (bs2[k] = False) do
      Inc(k);
    if s1[i] <> s2[k] then
      Inc(trans);
    Inc(k);
  end;
  trans := trans div 2;
  Result := ((matches / l1) + (matches / l2) + ((matches - trans) / matches)) / 3;
end;


end.
