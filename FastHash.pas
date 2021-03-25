unit FastHash;

interface

uses
  System.SysUtils;

//Uncomment if you want to test purepascal versions.
// {$define purepascal}

const
  DefaultHashSeed = 0;

function PascalMurmurHash3(const [ref] HashData; Len, Seed: integer): integer;
function MurmurHash3(const [ref] HashData; Len: integer; Seed: integer = 0)
 : integer;{$IFDEF purepascal}inline;{$ENDIF}
/// FNV1a_Hash_Meiyan is about 30% faster than murmurhash3
function FNV1A_Hash_Meiyan(const HashData; Len: integer; Seed: integer = DefaultHashSeed): integer;
{$IFDEF purepascal}inline;{$ENDIF}
function PascalFNV1A_Hash_Meiyan(const HashData; Len: integer; Seed: integer = DefaultHashSeed): integer;
function BobJenkinsHash(const HashData; Len, Seed: integer): integer; inline;
function xxHash32Calc(const HashData; Len: integer; Seed: integer = 0): integer;
function AsmxxHash32Calc(const HashData; Len: integer; Seed: integer = 0): integer;


implementation

uses
  System.Variants, System.Math, System.AnsiStrings, System.Generics.Defaults;

type
  PExtRec = ^TExtRec;
  TExtRec = packed record
    Mant: Int64;
    Exp: word
  end;

const
  SingleExpBits:  Integer = $7F800000; { 8 bits }
  SingleMantBits: Integer = $007FFFFF;
  DoubleExpBits:  Int64 = $7FF0000000000000; { 11 bits }
  DoubleMantBits: Int64 = $000FFFFFFFFFFFFF;
  ExtendedExpBits: word    = $7FFF;              {15 bits}
  ExtendedMantBits: Int64 = -1;

//function IsInfinite(const Value: double): boolean;
//begin
//  Result:= ((PInt64(@Value)^ and $7FF0000000000000) = $7FF0000000000000)
//       and ((PInt64(@Value)^ and $000FFFFFFFFFFFFF) = $0000000000000000)
//end;
Function IsNANSingle (const Value: single): boolean;  inline;
begin
  Result:= ((PInteger(@Value)^ and SingleExpBits) = SingleExpBits)
       and ((PInteger(@Value)^ and SingleExpBits) <> 0);
end;


function IsNanDouble(const Value: double): boolean; inline;
begin
  Result:= ((PInt64(@Value)^ and $7FF0000000000000) = $7FF0000000000000)
       and ((PInt64(@Value)^ and $000FFFFFFFFFFFFF) <> 0)
end;

{$if SizeOf(Extended) = 10}
function IsNanExtended(const Value: extended): boolean; inline;
begin
  Result:= ((PExtRec(@Value)^.Exp and ExtendedExpBits)= ExtendedExpBits)
       and ((PExtRec(@Value)^.Mant and ExtendedMantBits) <> 0);
end;
{$endif}

function IsZeroSingle(const Value: single): boolean; inline;
begin
  Result:= (PInteger(@Value)^ and $80000000) = 0;
end;

function IsZeroDouble(const Value: Double): boolean; inline;
begin
  Result:= (PInt64(@Value)^ and $000FFFFFFFFFFFFF = 0)
        or (PInt64(@Value)^ and $000FFFFFFFFFFFFF = $0008000000000000);
end;

{$if SizeOf(Extended) = 10}
function IsZeroExtended(const Value: Extended): boolean; inline;
begin
  Result:= (PExtRec(@Value)^.Exp = 0)
        or (PExtRec(@Value)^.Exp = $8000000000000000);
end;
{$endif}

function BobJenkinsHash(const HashData; Len, Seed: integer): integer;
begin
  Result:= System.Generics.Defaults.BobJenkinsHash(HashData, Len, Seed);
end;

(* From wikipedia:
  Murmur3_32(key, len, seed)
  // Note: In this version, all integer arithmetic is performed with unsigned 32 bit integers.
  //       In the case of overflow, the result is constrained by the application of modulo 2^{32} arithmetic.
  c1 := 0xcc9e2d51
  c2 := 0x1b873593
  r1 := 15
  r2 := 13
  m := 5
  n := 0xe6546b64

  hash := seed

  for each fourByteChunk of key
  k := fourByteChunk

  k := k * c1
  k := (k << r1) OR (k >> (32-r1))
  k := k * c2

  hash := hash XOR k
  hash := (hash << r2) OR (hash >> (32-r2))
  hash := hash * m + n

  with any remainingBytESInKey
  remainingBytes := remainingBytes * c1
  remainingBytes := (remainingBytes << r1) OR (remainingBytes >> (32 - r1))
  remainingBytes := remainingBytes * c2

  hash := hash XOR remainingBytes

  hash := hash XOR len

  hash := hash XOR (hash >> 16)
  hash := hash * 0x85ebca6b
  hash := hash XOR (hash >> 13)
  hash := hash * 0xc2b2ae35
  hash := hash XOR (hash >> 16)
  }(* *)

{$POINTERMATH on}

function PascalMurmurHash3(const [ref] HashData; Len, Seed: integer): integer;
const
  c1 = $CC9E2D51;
  c2 = $1B873593;
  r1 = 15;
  r2 = 13;
  m = 5;
  n = $E6546B64;
  f1 = $85EBCA6B;
  f2 = $C2B2AE35;
var
  i: NativeInt;
  Len1, Len2: integer;
  k: integer;
  data: PCardinal;
label case1, case2, case3, final;
type
  ByteArray = array[0 .. 0] of byte;
begin
  Result:= Seed;
  data:= @HashData;
  Len1:= (Len shr 2)- 1;
  for i:= 0 to Len1 do begin
    k:= data[i];
    k:= k * integer(c1);
    k:= (k shl r1) or (k shr (32 - r1));
    k:= k * c2;
    Result:= Result xor k;
    Result:= (Result shl r2) or (Result shr (32 - r2));
    Result:= Result * m + integer(n);
  end; { for i }
  k:= 0;
  Len2:= Len;
  case Len and $3 of
    1: goto case1;
    2: goto case2;
    3: goto case3;
  else goto final;
  end;
case3:
  dec(Len2);
  Inc(k, PByte(data)[Len2] shl 16);
case2:
  dec(Len2);
  Inc(k, PByte(data)[Len2] shl 8);
case1:
  dec(Len2);
  Inc(k, PByte(data)[Len2]);
  k:= k * integer(c1);
  k:= (k shl r1) or (k shr (32 - r1));
  k:= k * c2;
  Result:= Result xor k;
final:
  Result:= Result xor Len;

  Result:= Result xor (Result shr 16);
  Result:= Result * integer(f1);
  Result:= Result xor (Result shr 13);
  Result:= Result * integer(f2);
  Result:= Result xor (Result shr 16);
end;

function MurmurHash3(const [ref] HashData; Len: integer; Seed: integer = 0): integer;
{$IFDEF purepascal}
begin
  Result:= PascalMurmurHash3(HashData, Len, Seed);
end;
{$ELSE}
const
  c1 = $CC9E2D51;
  c2 = $1B873593;
  r1 = 15;
  r2 = 13;
  m = 5;
  n = $E6546B64;
  f1 = $85EBCA6B;
  f2 = $C2B2AE35;
{$REGION 'asm'}
{$IFDEF CPUx86}
asm
    push  EBX
    push  EDI
    push  ESI
    xchg  ECX,EDX
    mov   ESI,ECX
    // EAX = data
    // ECX = count in bytes
    // EDX = seed
    add   EAX, ECX
    neg   ECX
    add   ECX,4
    jg @remaining_bytes
  @loop:
    mov   EDI,[EAX+ECX-4]
    imul  EDI,EDI,c1
    rol   EDI,r1
    imul  EDI,EDI,c2
    xor   EDX,EDI
    rol   EDX,r2
    lea   EDX,[EDX*4+EDX+n]
    add   ECX,4
    jle @loop
  @remaining_bytes:
    cmp   ECX,4
    jz @finalization
    movzx EBX,byte ptr [EAX+ECX-4]
    cmp   ECX,3
    jz @process_remaining
    ror   EBX,8
    mov   BL,byte ptr [EAX+ECX-3]
    cmp   ECX,2// inc  RCX
    jz @shift_back
    ror   EBX,8// ror  R9d,24
    mov   BL,byte ptr [EAX+ECX-2]
    rol   EBX,8
  @shift_back:
    rol   EBX,8
  @process_remaining:
    imul  EBX,EBX,c1
    rol   EBX,r1
    imul  EBX,EBX,c2
    xor   EDX,EBX
  @finalization:
    xor   EDX,ESI
    mov   EAX,EDX
    shr   EDX,16
    xor   EDX,EAX
    imul  EDX,EDX,f1
    mov   EAX,EDX
    shr   EDX,13
    xor   EDX,EAX
    imul  EDX,EDX,f2
    mov   EAX,EDX
    shr   EDX,16
    xor   EAX,EDX
    pop   ESI
    pop   EDI
    pop   EBX
end;
{$ENDIF}
{$IFDEF CPUx64}
asm
    .NOFRAME
    xchg  R10,RDI
    mov   RAX,RCX
    lea   RCX,[RDX-4]
    mov   R11,RDX
    mov   RDX,R8
    // RAX = data
    // RCX = count in bytes
    // RDX = seed
    // make index negative based.
    lea   RAX, [RAX+RCX+4]
    neg   RCX
    jg @remaining_bytes
@loop:
    mov   EDI,[RAX+RCX-4]
    imul  EDI,EDI,c1
    rol   EDI,r1
    imul  EDI,EDI,c2
    xor   EDX,EDI
    rol   EDX,r2
    lea   EDX,[EDX*4+EDX+n] // *5 + n
    // lea  RAX,qword ptr [RAX+4]
    add   RCX,4
    jle @loop
@remaining_bytes:
    cmp   RCX,4
    // mov  ECX,ER11
    // and  ER11,$3
    jz @finalization
    movzx R9,byte ptr [RAX+RCX-4]
    cmp   RCX,3// inc  RCX
    jz @process_remaining
    ror   R9,8// ror  R9d,24
    mov   R9b,byte ptr [RAX+RCX-3]
    cmp   RCX,2// inc  RCX
    jz @shift_back
    ror   R9,8// ror  R9d,24
    mov   R9b,byte ptr [RAX+RCX-2]
    rol   R9,8
@shift_back:
    rol   R9,8
@process_remaining:
    imul  R9d,R9d,c1
    rol   R9d,r1
    imul  R9d,R9d,c2
    xor   EDX,R9d
@finalization:
    xor   EDX,R11d
    mov   EAX,EDX
    shr   EDX,16
    xor   EDX,EAX
    imul  EDX,EDX,f1
    mov   EAX,EDX
    shr   EDX,13
    xor   EDX,EAX
    imul  EDX,EDX,f2
    mov   EAX,EDX
    shr   EDX,16
    xor   EAX,EDX
    xchg  R10,RDI
end;
{$ENDIF}
{$ENDREGION}
{$ENDIF}
{$POINTERMATH on}

{$WARNINGS OFF}
// Meiyan means Beauty, Charming Eyes or most precisely: SOULFUL EYES.
function PascalFNV1A_Hash_Meiyan(const HashData; Len: integer; Seed: integer = 0): integer;
const
  //prime = 709607;
  prime = 16777619;
  offset_basis = 2166136261;
  n = $E6546B64;
var
  Hash32: integer;
  p: PInteger;
  Tail: integer;
  Temp: integer;
begin
  p:= @HashData;
  Hash32:= Seed + offset_basis;
  Tail:= Hash32;
  while Len >= 2 * SizeOf(cardinal) do begin
    Hash32:= (Hash32 xor (((p[0] shl 5) or (p[0] shr (32 - 5))) xor p[1]));
    Tail:= Tail xor Hash32;
    Hash32:= Hash32 * prime;
    Inc(p, 2);
    dec(Len, SizeOf(integer)* 2)
  end;
  if (Len and SizeOf(integer)) <> 0 then begin
    Temp:= p^;
    Tail:= Tail xor p^;
    Hash32:= (Hash32 xor (temp and $FFFF)) * prime;
    Tail:= Tail xor Hash32;
    Hash32:= (Hash32 xor (temp shr 16)) * prime;
    Inc(p);
  end;
  if (Len and SizeOf(word)) <> 0 then begin
    Temp:= PWord(p)^;
    Tail:= Tail xor Temp;
    Hash32:= (Hash32 xor Temp) * prime;
    Inc(PWord(p));
  end;
  if (Len and 1) <> 0 then begin
    Temp:= PByte(p)^;
    Tail:= Tail xor Temp;
    Hash32:= (Hash32 xor Temp) * prime;
  end;
  //Extra shuffle
  Tail:= (Tail * 17) + n;
  //Hash32:= Hash32 + Tail;
  Result:= (Hash32 + Tail) xor (Hash32 shr 16);
end;
{$POINTERMATH off}

function FNV1A_Hash_Meiyan(const HashData; Len: integer; Seed: integer = 0): integer;
{$IFDEF PurePascal}
begin
  Result:= PascalFNV1A_Hash_Meiyan(HashData, Len, Seed);
end;
{$ELSE}

const
  //prime = 709607;
  prime = 16777619;
  n = $E6546B64;
  offset_basis = 2166136261;
  {$REGION 'asm'}
  {$IFDEF CPUX86}
  asm
    // EAX = STR
    // EDX = len
    // ECX = seed
    push  EBX
    push  ESI
    push  EDI
    add   ECX, offset_basis
    add   EAX,EDX
    lea   ESI,[EDX-8]
    neg   ESI
    jg @remaining
  @Loop8:
    mov   EBX,[EAX+ESI-8]
    rol   EBX,5

    xor   EBX,[EAX+ESI-8+4]
    xor   ECX,EBX
    mov   EDI,ECX
    imul  ECX,ECX,prime
    add   ESI,$08
    jle @loop8
  @remaining:
    lea   ESI,[ESI+EAX-8]
    test  DL,$4
    jz @wordsize

    mov   EBX,[ESI]
    mov   EDI,EBX
    mov   EAX,EBX
    and   EBX,$ffff
    xor   ECX,EBX
    imul  ECX,ECX,prime

    shr   EAX,16
    xor   ECX,EAX
    imul  ECX,ECX,prime

    add   ESI,$04
  @wordsize:
    test  DL,2
    jz @bytESIze

    movzx EBX, word ptr [ESI]
    mov   EDI,EBX
    xor   ECX,EBX
    imul  ECX,ECX,prime

    add   ESI,$02
  @bytesize:
    test  DL,1
    jz @wrapup

    movzx EBX, byte ptr [ESI]
    mov   EDI,EBX
    xor   ECX,EBX
    imul  ECX,ECX,prime
@wrapup:
//  @wrapup:
//    //Reduce collisions for short keys.
//    //The extra instructions are hidden in the latency of imul
    lea   EAX,[EDI*8]
    lea   EBX,[EAX*2+EDI+n]
    lea   EAX,[EBX+ECX]
    shr   ECX,16
    xor   EAX,ECX
    pop   EDI
    pop   ESI
    pop   EBX
end;
{$ENDIF}
{$IFDEF CPUX64}
asm
    .NOFRAME
    // ECX = STR
    // EDX = len
    // R8 = seed
    add   RCX,RDX
    add   R8d,offset_basis
    lea   R11,[RDX-8]
    neg   R11
    mov   R10d,R8d
    jg @remaining
@Loop8:
    //Hash32:= (Hash32 xor (p[0] rol 5) xor p[1])) * prime;
    //Inc(p, 2);
    //dec(Len, SizeOf(integer)* 2)
    mov   RAX,[RCX+R11-8]
    mov   R9,RAX
    rol   R9d,5
    shr   RAX,32
    xor   EAX,R9d
    xor   R8d,EAX
    //Tail:= Tail xor Hash32
    xor   R10d,R8d
    imul  R8d,R8d,prime
    add   R11,$08
    jle @loop8
@remaining:
    //xor   R10,R10
    lea   R11,[R11+RCX-8]
    test  DL,$4
    jz @wordsize
//    Tail:= p^;
    mov   EAX,[R11]
//    Hash32:= (Hash32 xor (Tail and $FFFF)) * prime;
    xor   R10d, EAX
    mov   R9d,EAX
    and   R9d,$ffff
    xor   R8d,R9d
    imul  R8d,R8d,prime
//    Hash32:= (Hash32 xor (Tail shr 16)) * prime;
    //mov   R10d,EAX
    xor   R10d,R8d
    shr   EAX,16
    xor   R8d,EAX
    imul  R8d,R8d,prime
//    Inc(PWord(p));
    add   R11,$04
@wordsize:
    test  DL,2
    jz @bytesize

    movzx R9d, word ptr [R11]
    xor   R8d,R9d
    xor   R10d,R9d
    imul  R8d,R8d,prime

    add   R11,$02
@bytesize:
    test DL,1
    jz @wrapup

    movzx R9d, byte ptr [R11]
    xor   R8d,R9d
    xor   R10d,R9d
    imul  R8d,R8d,prime
@wrapup:
    lea   EAX,[R10d*8]
    lea   ECX,[EAX*2+R10d+n]
    lea   EAX,[ECX+R8d]
    shr   R8d,16
    xor   EAX,R8d
end;
{$ENDIF}{$ENDIF}
{$ENDREGION}

const
  cPrime32x1 = 2654435761;
	cPrime32x2 = 2246822519;
	cPrime32x3 = 3266489917;
	cPrime32x4 = 668265263;
	cPrime32x5 = 374761393;

  cPrime64x1 = 11400714785074694791;
  cPrime64x2 = 14029467366897019727;
  cPrime64x3 = 1609587929392839161;
  cPrime64x4 = 9650029242287828579;
  cPrime64x5 = 2870177450012600261;


function rol(Input: integer; shift: integer): integer; overload; inline;
begin
  Result:= (Input shl shift) or (Input shr (32 - shift));
end;

function rol64(Input: Int64; shift: integer): Int64; overload; inline;
begin
  Result:= (Input shl Shift) or (Input shr (64 - Shift));
end;

{$PointerMath On}
function xxHash32Calc(const HashData; Len: integer; Seed: integer = 0): integer;
var
  v1, v2, v3, v4: Cardinal;
  pLimit, pEnd, ABuffer: pointer;
begin
  ABuffer:= @HashData;
  pEnd:= pointer(NativeUInt(ABuffer) + Len);
  if Len >= 16 then begin
    pLimit:= pointer(NativeUInt(pEnd) - 16);
    v1:= Seed + cPrime32x1 + cPrime32x2;
    v2:= Seed + cPrime32x2;
    v3:= Seed;
    v4:= Seed - cPrime32x1;

    repeat
      v1:= cPrime32x1 * rol(v1 + cPrime32x2 * PCardinal(ABuffer)^, 13);
      v2:= cPrime32x1 * rol(v2 + cPrime32x2 * PCardinal(NativeUInt(ABuffer)+ 4)^, 13);
      v3:= cPrime32x1 * rol(v3 + cPrime32x2 * PCardinal(NativeUInt(ABuffer)+ 8)^, 13);
      v4:= cPrime32x1 * rol(v4 + cPrime32x2 * PCardinal(NativeUInt(ABuffer)+ 12)^, 13);
      Inc(NativeUInt(ABuffer), 16);
    until not(NativeUInt(ABuffer) <= NativeUInt(pLimit));

    Result:= Rol(v1, 1) + Rol(v2, 7) + Rol(v3, 12) + Rol(v4, 18);
  end
  else Result:= Seed + cPrime32x5;

  Inc(Result, Len);

  while NativeUInt(ABuffer) <= (NativeUInt(pEnd) - 4) do begin
    Result:= Result + PCardinal(ABuffer)^ * cPrime32x3;
    Result:= Rol(Result, 17) * cPrime32x4;
    Inc(NativeUInt(ABuffer), 4);
  end;

  while NativeUInt(ABuffer) < NativeUInt(pEnd) do begin
    Result:= Result + PByte(ABuffer)^ * cPrime32x5;
    Result:= Rol(Result, 11) * cPrime32x1;
    Inc(NativeUint(ABuffer));
  end;

  Result:= Result xor (Result shr 15);
  Result:= Result * cPrime32x2;
  Result:= Result xor (Result shr 13);
  Result:= Result * cPrime32x3;
  Result:= Result xor (Result shr 16);
end;
{$WARNINGS ON}

function AsmxxHash32Calc(const HashData; Len: integer; Seed: integer = 0): integer;
{$ifdef CPUX64}
asm
     //xxHashTest.dpr.124: begin
     push rsi
     push rbx
     //xxHashTest.dpr.125: ABuffer:= @HashData;
     mov rbx,rcx
     //xxHashTest.dpr.126: pEnd:= pointer(NativeUInt(ABuffer) + Len);
     //movsxd rax,edx
     lea r11,[rcx+rdx]
     //xxHashTest.dpr.127: if Len >= 16 then begin
     cmp edx,$10
     jl @SmallHash
     //xxHashTest.dpr.128: pLimit:= pointer(NativeUInt(pEnd) - 16);
     lea r10,[rcx+rdx-$10]
     //xxHashTest.dpr.129: v1:= Seed + cPrime32x1 + cPrime32x2;
     lea eax,[r8d+$24234428]
     //xxHashTest.dpr.130: v2:= Seed + cPrime32x2;
     lea ecx,[r8d+$85ebca77]
     //xxHashTest.dpr.131: v3:= Seed;
     mov r9d,r8d
     //xxHashTest.dpr.132: v4:= Seed - cPrime32x1;
     add r8d,$61c8864f
     //xxHashTest.dpr.135: v1:= cPrime32x1 * rol(v1 + cPrime32x2 * PCardinal(ABuffer)^, 13);
@StartRepeat16:
     imul esi,[rbx],$85ebca77
     add eax,esi
     rol eax,$0d
     imul eax,eax,$9e3779b1
     //xxHashTest.dpr.136: v2:= cPrime32x1 * rol(v2 + cPrime32x2 * PCardinal(NativeUInt(ABuffer)+ 4)^, 13);
     imul esi,[rbx+$04],$85ebca77
     add ecx,esi
     rol ecx,$0d
     imul ecx,ecx,$9e3779b1
     //xxHashTest.dpr.137: v3:= cPrime32x1 * rol(v3 + cPrime32x2 * PCardinal(NativeUInt(ABuffer)+ 8)^, 13);
     imul esi,[rbx+$08],$85ebca77
     add r9d,esi
     rol r9d,$0d
     imul r9d,r9d,$9e3779b1
     //xxHashTest.dpr.138: v4:= cPrime32x1 * rol(v4 + cPrime32x2 * PCardinal(NativeUInt(ABuffer)+ 12)^, 13);
     imul esi,[rbx+$0c],$85ebca77
     add r8d,esi
     rol r8d,$0d
     imul r8d,r8d,$9e3779b1
     //xxHashTest.dpr.139: Inc(NativeUInt(ABuffer), 16);
     add rbx,$10
     //xxHashTest.dpr.140: until not(NativeUInt(ABuffer) <= NativeUInt(pLimit));
     cmp rbx,r10
     jbe @StartRepeat16
     //xxHashTest.dpr.142: Result:= Rol(v1, 1) + Rol(v2, 7) + Rol(v3, 12) + Rol(v4, 18);
     rol eax,1
     rol ecx,$07
     add eax,ecx
     rol r9d,$0c
     add eax,r9d
     rol r8d,$12
     add eax,r8d
     jmp @Tail
@SmallHash:
     //xxHashTest.dpr.144: else Result:= Seed + cPrime32x5;
     lea eax,[r8d+$165667b1]
     //xxHashTest.dpr.146: Inc(Result, Len);
@Tail:
     add eax,edx
     jmp @EndWhile4Bytes
@StartWhile4Bytes:
     //xxHashTest.dpr.149: Result:= Result + PCardinal(ABuffer)^ * cPrime32x3;
     imul ecx,[rbx],$c2b2ae3d
     add ecx,eax
     //xxHashTest.dpr.150: Result:= Rol(Result, 17) * cPrime32x4;
     //mov ecx,eax
     rol ecx,$11
     imul eax,ecx,$27d4eb2f
     //mov eax,ecx
     //xxHashTest.dpr.151: Inc(NativeUInt(ABuffer), 4);
     add rbx,$04
     //xxHashTest.dpr.148: while NativeUInt(ABuffer) <= (NativeUInt(pEnd) - 4) do begin
@EndWhile4Bytes:
     lea rcx,[r11-$04]
     cmp rbx,rcx
     jbe @StartWhile4Bytes
     jmp @EndWhileBytes
     //xxHashTest.dpr.155: Result:= Result + PByte(ABuffer)^ * cPrime32x5;
@StartWhileBytes:
     movzx rcx,byte ptr [rbx]
     imul ecx,ecx,$165667b1
     add eax,ecx
     //xxHashTest.dpr.156: Result:= Rol(Result, 11) * cPrime32x1;
     //mov ecx,eax
     rol eax,$0b
     imul eax,eax,$9e3779b1
     //mov eax,ecx
     //xxHashTest.dpr.157: Inc(NativeUint(ABuffer));
     add rbx,$01
     //xxHashTest.dpr.154: while NativeUInt(ABuffer) < NativeUInt(pEnd) do begin
@EndWhileBytes:
     cmp rbx,r11
     jb @StartWhileBytes
     //xxHashTest.dpr.160: Result:= Result xor (Result shr 15);
     mov ecx,eax
     shr ecx,$0f
     xor eax,ecx
     //xxHashTest.dpr.161: Result:= Result * cPrime32x2;
     imul eax,eax,$85ebca77
     //xxHashTest.dpr.162: Result:= Result xor (Result shr 13);
     mov ecx,eax
     shr ecx,$0d
     xor eax,ecx
     //xxHashTest.dpr.163: Result:= Result * cPrime32x3;
     imul eax,eax,$c2b2ae3d
     //xxHashTest.dpr.164: Result:= Result xor (Result shr 16);
     mov ecx,eax
     shr ecx,$10
     xor eax,ecx
     //xxHashTest.dpr.165: end;
     pop rbx
     pop rsi
end;
{$elseif defined(CPUX86)}
asm
      //.1889: begin
     push ebx
     push esi
     push edi
     push ebp
     add esp,-$14
     mov [esp],edx
      //.1890: ABuffer:= @HashData;
     mov ebx,eax
      //.1891: pEnd:= pointer(NativeUInt(ABuffer) + Len);
     lea esi,[eax+edx]
      //.1892: if Len >= 16 then begin
     cmp edx,16
     jl @SmallHash
      //.1893: pLimit:= pointer(NativeUInt(pEnd) - 16);
     mov ebp,ecx
     sub esi,16
      //.1894: v1:= Seed + cPrime32x1 + cPrime32x2;
     lea eax,[ebp+$9e3779b1+$85ebca77]
      //.1895: v2:= Seed + cPrime32x2;
     lea edx,[ebp+$85ebca77]
      //.1896: v3:= Seed;
     //mov ebp,[esp+$04]
     //mov [esp+$08],ecx
      //.1897: v4:= Seed - cPrime32x1;
     lea edi,[ebp-$9e3779b1]
@StartRepeat16:
      //.1900: v1:= cPrime32x1 * rol(v1 + cPrime32x2 * PCardinal(ABuffer)^, 13);
     imul ecx,[ebx],$85ebca77
     add eax,ecx
     rol eax,$0d
     imul eax,eax,$9e3779b1
      //.1901: v2:= cPrime32x1 * rol(v2 + cPrime32x2 * PCardinal(NativeUInt(ABuffer)+ 4)^, 13);
     imul ecx,[ebx+4],$85ebca77
     add edx,ecx
     rol edx,$0d
     imul edx,edx,$9e3779b1
      //.1902: v3:= cPrime32x1 * rol(v3 + cPrime32x2 * PCardinal(NativeUInt(ABuffer)+ 8)^, 13);
     imul ecx,[ebx+8],$85ebca77
     add ecx,ebp
     rol ecx,$0d
     imul ecx,ecx,$9e3779b1
     mov ebp,ecx
      //.1903: v4:= cPrime32x1 * rol(v4 + cPrime32x2 * PCardinal(NativeUInt(ABuffer)+ 12)^, 13);
     imul ecx,[ebx+12],$85ebca77
     add ecx,edi
     rol ecx,$0d
     imul edi,ecx,$9e3779b1
      //.1904: Inc(NativeUInt(ABuffer), 16);
     add ebx,16
      //.1905: until not(NativeUInt(ABuffer) <= NativeUInt(pLimit));
     cmp ebx,esi
     jbe @StartRepeat16
      //.1907: Result:= Rol(v1, 1) + Rol(v2, 7) + Rol(v3, 12) + Rol(v4, 18);
     add esi,$10
     rol eax,1
     rol edx,$07
     rol ebp,$0c
     rol edi,$12
     add eax,edx
     add edi,ebp
     add eax,edi
     jmp @Tail
@SmallHash:
      //.1909: else Result:= Seed + cPrime32x5;
     lea eax,[ecx+$165667b1]
@Tail:
      //.1911: Inc(Result, Len);
     lea ebp,[esi-4]
     add eax,[esp]
     cmp ebp,ebx
     jb @WhileBytes
@StartWhile4Bytes:
      //.1914: Result:= Result + PCardinal(ABuffer)^ * cPrime32x3;
     imul edx,[ebx],$c2b2ae3d
     add edx,eax
      //.1915: Result:= Rol(Result, 17) * cPrime32x4;
     rol edx,$11
     imul eax,edx,$27d4eb2f
      //.1916: Inc(NativeUInt(ABuffer), 4);
     add ebx,$04
      //.1913: while NativeUInt(ABuffer) <= (NativeUInt(pEnd) - 4) do begin
@EndWhile4Bytes:
     lea ebp,[esi-4]
     cmp ebp,ebx
     jnb @StartWhile4Bytes
@WhileBytes:
     cmp ebx,esi
     jnb @Finalization
      //.1920: Result:= Result + PByte(ABuffer)^ * cPrime32x5;
@StartWhileBytes:
     movzx edx, byte ptr [ebx]
     imul edx,edx,$165667b1
     add edx,eax
      //.1921: Result:= Rol(Result, 11) * cPrime32x1;
     rol edx,$0b
     imul eax,edx,$9e3779b1
      //.1922: Inc(NativeUint(ABuffer));
     inc ebx
      //.1919: while NativeUInt(ABuffer) < NativeUInt(pEnd) do begin
@EndWhileBytes:
     cmp ebx,esi
     jb @StartWhileBytes
      //.1925: Result:= Result xor (Result shr 15);
@Finalization:
     mov edx,eax
     shr eax,$0f
     xor eax,edx
      //.1926: Result:= Result * cPrime32x2;
     imul eax,eax,$85ebca77
      //.1927: Result:= Result xor (Result shr 13);
     mov edx,eax
     shr eax,$0d
     xor eax,edx
      //.1928: Result:= Result * cPrime32x3;
     imul eax,eax,$c2b2ae3d
      //.1929: Result:= Result xor (Result shr 16);
     mov edx,eax
     shr eax,$10
     xor eax,edx
      //.1930: end;
     add esp,$14
     pop ebp
     pop edi
     pop esi
     pop ebx
end;
{$endif}

function xxHash64Calc(const HashData; Len: integer; Seed: Uint64 = 0): UInt64;
var
  v1, v2, v3, v4: UInt64;
  pLimit, pEnd, ABuffer: Pointer;
begin
  ABuffer:= @HashData;
  Nativeint(pEnd):= NativeInt(ABuffer) + Len;

  if Len >= 32 then begin
    v1:= Seed + cPrime64x1 + cPrime64x2;
    v2:= Seed + cPrime64x2;
    v3:= Seed;
    v4:= Seed - cPrime64x1;

    NativeUInt(pLimit):= NativeUInt(pEnd) - 32;
    repeat
      v1:= cPrime64x1 * rol64(v1 + cPrime64x2 * PUInt64(ABuffer)^, 31);
      v2:= cPrime64x1 * rol64(v2 + cPrime64x2 * PUInt64(NativeUInt(ABuffer) + 8)^, 31);
      v3:= cPrime64x1 * rol64(v3 + cPrime64x2 * PUInt64(NativeUInt(ABuffer) + 16)^, 31);
      v4:= cPrime64x1 * rol64(v4 + cPrime64x2 * PUInt64(NativeUInt(ABuffer) + 24)^, 31);
      Inc(NativeUInt(ABuffer), 32);
    until not(NativeUInt(ABuffer) <= NativeUInt(pLimit));

    Result:= rol64(v1, 1) + rol64(v2, 7) + rol64(v3, 12) + rol64(v4, 18);

    v1:= rol64(v1 * cPrime64x2, 31) * cPrime64x1;
    Result:= (Result xor v1) * cPrime64x1 + cPrime64x4;

    v2:= rol64(v2 * cPrime64x2, 31) * cPrime64x1;
    Result:= (Result xor v2) * cPrime64x1 + cPrime64x4;

    v3:= rol64(v3 * cPrime64x2, 31) * cPrime64x1;
    Result:= (Result xor v3) * cPrime64x1 + cPrime64x4;

    v4:= rol64(v4 * cPrime64x2, 31) * cPrime64x1;
    Result:= (Result xor v4) * cPrime64x1 + cPrime64x4;
  end
  else Result:= Seed + cPrime64x5;

  Inc(Result, Len);

  while NativeUInt(ABuffer) <= (NativeUInt(pEnd) - 8) do begin
    Result:= Result xor (cPrime64x1 * rol64(cPrime64x2 * PUInt64(ABuffer)^, 31));
    Result:= rol64(Result, 27) * cPrime64x1 + cPrime64x4;
    Inc(NativeUInt(ABuffer), 8);
  end;

  if NativeUInt(ABuffer) <= (NativeUInt(pEnd) - 4) then begin
    Result:= Result xor (PCardinal(ABuffer)^ * cPrime64x1);
    Result:= rol64(Result, 23) * cPrime64x2 + cPrime64x3;
    Inc(NativeUInt(ABuffer), 4);
  end;

  while NativeUInt(ABuffer) < NativeUInt(pEnd) do begin
    Result:= Result xor (PByte(ABuffer)^ * cPrime64x5);
    Result:= rol64(Result, 11) * cPrime64x1;
    Inc(NativeUInt(ABuffer));
  end;

  Result:= Result xor (Result shr 33);
  Result:= Result * cPrime64x2;
  Result:= Result xor (Result shr 29);
  Result:= Result * cPrime64x3;
  Result:= Result xor (Result shr 32);
end;
{$PointerMath Off}


end.
