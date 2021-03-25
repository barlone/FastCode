unit FastCompare;

interface

uses
  System.SysUtils;

//Uncomment if you want to test purepascal versions.
// {$define purepascal}

const
  DefaultHashSeed = 0;

function CompareFast(const Left, Right: byte): integer; overload; inline;
function CompareFast(const Left, Right: int8): integer; overload; inline;
function CompareFast(const Left, Right: word): integer; overload; inline;
function CompareFast(const Left, Right: int16): integer; overload; inline;
function CompareFast(const Left, Right: cardinal): integer; overload; inline;
function CompareFast(const Left, Right: integer): integer; overload; inline;
function CompareFast(const Left, Right: UInt64): integer; overload; inline;
function CompareFast(const Left, Right: Int64): integer; overload; inline;
function CompareFast(const Left, Right: NativeInt): integer; overload; inline;
function CompareFast(const Left, Right: NativeUInt): integer; overload; inline;
function CompareFast(const Left, Right: AnsiChar): integer; overload; inline;
function CompareFast(const Left, Right: WideChar): integer; overload; inline;
function CompareFast(const Left, Right: UCS4Char): integer; overload; inline;
function CompareFast(const Left, Right: single): integer; overload; inline;
function CompareFast(const Left, Right: Real48): integer; overload; inline;
function CompareFast(const Left, Right: double): integer; overload; inline;
function CompareFast(const Left, Right: extended): integer; overload; inline;
function CompareFast(const Left, Right: Comp): integer; overload; inline;
function CompareFast(const Left, Right: Currency): integer; overload; inline;
function CompareFast(const Left, Right: boolean): integer; overload; inline;
function CompareFast(const Left, Right: wordbool): integer; overload; inline;
function CompareFast(const Left, Right: longbool): integer; overload; inline;
function CompareFast(const Left, Right: bytebool): integer; overload; inline;
function CompareFast(const Left, Right: ShortString): integer; overload; inline;
function CompareFast(const Left, Right: AnsiString): integer; overload; inline;
function CompareFast(const Left, Right: UnicodeString): integer; overload; inline;
function CompareFast(const Left, Right: WideString): integer; overload; inline;
function CompareFast(const Left, Right: RawByteString): integer; overload; inline;
function CompareFast(const Left, Right: UTF8String): integer; overload; inline;
function CompareFast(const Left, Right: pointer): integer; overload; inline;
function CompareFast(const Left, Right: IInterface): integer; overload; inline;
function CompareFast(const Left, Right: TObject): integer; overload; inline;

function Compare_Variant(const Left, Right: pointer): integer;
function BinaryCompare(const Left, Right; Size: integer): integer;
function FastBinaryCompare(const Left, Right; Size: integer): integer;
function BinaryCompare4(const Left, Right: cardinal): integer;
{$IFDEF purepascal}
function BinaryCompare8(const Left, Right: pointer): integer;
{$ELSEIF DEFINED(CPUX64)}
function BinaryCompare8(const Left, Right: UInt64): integer;
{$ELSE !CPUX64}
function BinaryCompare8(const Left, Right: pointer): integer;
{$ENDIF}
function BinaryCompare3(const Left, Right: pointer): integer;
function Compare_DynArray(const Left, Right: pointer; ElementSize: integer): NativeInt;
function Compare_PSn(const Left, Right: OpenString): integer;
{$IFDEF purepascal}inline;{$ELSE}{$IFNDEF CPUX64}inline;{$ENDIF}{$ENDIF}

function CompareWideStr(const S1, S2: WideString): integer;
function CompareUnicodeStr(const S1, S2: string): integer;
function CompareAnsiStr(const S1, S2: AnsiString): integer;
function SameUnicodeStr(const S1, S2: string): boolean;
function SameWideStr(const S1, S2: WideString): boolean;
function BinaryEquals(const Left, Right: pointer; Size: integer): boolean;
function DynLen(const Arr: pointer): NativeInt; inline;

function Normalize(const X: single): single; overload;
//function Normalize(const X: Real48): Real48; overload; //Real48 is always normalized
function Normalize(const X: double): double; overload;
{$if SizeOf(Extended) = 10}
function Normalize(const X: extended): extended; overload;
{$endif}




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

function Normalize(const X: single): single; overload;
begin
  if IsNanSingle(x) then Result:= NaN
  else if IsZeroSingle(x) then Result:= 0.0
  else Result:= x;
end;

//The Real48 type cannot store denormals, NaNs, and infinities (Inf).
//Denormals become zero when stored in a Real48,
//while NaNs and infinities produce an overflow error if an attempt is made
//to store them in a Real48.
//function Normalize(const X: Real48): Real48; overload;

function Normalize(const X: double): double; overload;
begin
  if IsNanDouble(x) then Result:= NaN
  else if IsZeroDouble(x) then Result:= 0.0
  else Result:= x;
end;

{$if SizeOf(Extended) = 10}
function Normalize(const X: extended): extended; overload;
begin
  if IsNanExtended(x) then Result:= NaN
  else if IsZeroExtended(x) then Result:= 0.0
  else Result:= x;
end;
{$endif}

function Compare_Variant(const Left, Right: pointer): integer;
var
  l, r: Variant;
  lAsString, rAsString: string;
  varTypeLeft, varTypeRight: integer;
begin
  Result:= 0; // Avoid warning.
  l:= PVariant(Left)^;
  r:= PVariant(Right)^;
  try
    case System.Variants.VarCompareValue(l, r) of
      vrEqual: Exit(0);
      vrLessThan: Exit( -1);
      vrGreaterThan: Exit(1);
      vrNotEqual: begin
          if VarIsEmpty(l) or VarIsNull(l) then Exit(1)
          else Exit( -1);
        end;
    end;
  except // if comparison failed with exception, compare as string.
    try
      // Prevent type conversions
      varTypeLeft:= VarType(l) and varTypeMask;
      varTypeRight:= VarType(r) and varTypeMask;
      if ((varTypeLeft = varUString) or (varTypeRight = varUString)) or
        ((varTypeLeft = varOleStr) or (varTypeRight = varOleStr)) then begin
        Result:= CompareWideStr(WideString(l), WideString(r));
      end else if (varTypeLeft = varString) or (varTypeRight = varString) then begin
        Result:= CompareAnsiStr(AnsiString(l), AnsiString(r));
      end else begin
        lAsString:= PVariant(Left)^;
        rAsString:= PVariant(Right)^;
        Result:= CompareUnicodeStr(lAsString, rAsString);
      end;
    except // if comparison fails again, compare bytes.
      Result:= FastBinaryCompare(Left, Right, SizeOf(Variant));
    end;
  end;
end;

function BinaryCompare4(const Left, Right: cardinal): integer;
{$IFDEF purepascal}
var
  i: integer;
begin
  Result:= (integer(Left > Right) - integer(Left < Right));
end;
{$ELSE !PurePascal}
{$IFDEF CPUX64}
asm
    .NOFRAME
    // Left: RCX
    // Right: RDX
    // preserve:RBX, RBP, RDI, RSI, R12-R15
    // bswap ECX
    // bswap EDX
    cmp   ECX,EDX
    sbb   EAX,EAX
    cmp   EDX,ECX
    adc   EAX,0
end;
{$ENDIF}
{$IFDEF CPUX86}
asm
    // Left: EAX
    // Right: EDX
    // Size: ECX
    cmp   EAX,EDX
    seta  AL
    movzx EAX,AL
    sbb   EAX,0
end;
{$ENDIF}
{$ENDIF !PurePascal}
{$IFDEF purepascal}

function BinaryCompare8(const Left, Right: pointer): integer;
var
  pl, pr: PByte;
  i: integer;
begin
  pl:= Left;
  pr:= Right;
  for i:= 0 to 7 do begin
    Result:= pl^ - pr^;
    if Result <> 0 then Exit;
    Inc(pl);
    Inc(pr);
  end;
  Result:= 0;
end;
{$ELSE !PurePascal}
{$IFDEF CPUX64}
function BinaryCompare8(const Left, Right: UInt64): integer;
asm
    .NOFRAME
    // Left: RCX
    // Right: RDX
    // preserve:RBX, RBP, RDI, RSI, R12-R15
    bswap RCX
    bswap RDX
    xor   EAX,EAX
    cmp   RCX,RDX
    seta  AL
    sbb   EAX,0
end;
{$ENDIF}
{$IFDEF CPUX86}

function BinaryCompare8(const Left, Right: pointer): integer;
asm
    // Left: EAX
    // Right: EDX
    // Size: ECX
    push  EBX
    mov   EBX, [EAX]
    mov   ECX, [EDX]
    bswap EBX
    bswap ECX
    cmp   EBX, ECX
    jnz @done
    mov   EBX, [EAX+4]
    mov   ECX, [EDX+4]
    bswap EBX
    bswap ECX
    cmp   EBX, ECX
@done:
    sbb   EAX,EAX
    cmp   ECX,EBX
    adc   EAX,0
    pop   EBX
end;
{$ENDIF}
{$ENDIF !PurePascal}

function BinaryCompare3(const Left, Right: pointer): integer;
{$IFDEF PurePascal}
var
  pl, pr: PByte;
  i: integer;
begin
  pl:= Left;
  pr:= Right;
  for i:= 0 to 2 do begin
    Result:= pl^ - pr^;
    if Result <> 0 then Exit;
    Inc(pl);
    Inc(pr);
  end;
  Result:= 0;
end;
{$ELSE !PurePascal}
{$IFDEF CPUX64}
asm  // Left RCX, Right RDX
    movzx R8,  word ptr [RCX]
    shl   R8,  16
    mov   R8b, byte ptr [RCX+2]
    bswap R8
    movzx R9,  word ptr [RCX]
    shl   R9,  16
    mov   R9b, byte ptr [RCX+2]
    bswap R9
    cmp   R8,  R9
    sbb   EAX, EAX
    cmp   R9,  R8
    adc   EAX, 0
end;
{$ELSE CPUX86}
asm
    push  EBX
    movzx EBX, word ptr [EAX]
    shl   EBX, 16
    mov   BL,  byte ptr [EAX+2]
    bswap EBX
    movzx ECX, word ptr [EDX]
    shl   ECX, 16
    mov   CL,  byte ptr [EDX+2]
    bswap ECX
    cmp   EBX, ECX
    sbb   EAX, EAX
    cmp   ECX, EBX
    adc   EAX, 0
    pop   EBX
end;
{$ENDIF}{$ENDIF}

// Size is not 1,2,3,4 or 8
function BinaryCompare(const Left, Right; Size: integer): integer;
{$IFDEF purepascal} // pure pascal
type
  TBSwap = record
    case integer of
      1: (Arr: array[0 .. SizeOf(NativeInt)- 1] of byte);
      2: (ni: NativeInt);
      3: (i: integer);
  end;
var
  Same: boolean;
  i: integer;
  Lq, Rq: PNativeUInt;
  Li, Ri: PInteger;
  SwapL, SwapR: TBSwap;
label
  DifferentInteger, DifferentNativeInt;
begin
  if (@Left = @Right) then Exit(0)
  else if @Left = nil then Exit(-1)
  else if @Right = nil then Exit(1);
  case Size of
    0: Result:= 0;
    1: Result:= PByte(@Left)^ - PByte(@Right)^;
    2, 3: begin
        for i:= 0 to Size - 1 do begin
          Result:= PByte(@Left)[i] - PByte(@Right)[i];
          if Result <> 0 then Exit;
        end;
      end
  else begin
      if (SizeOf(NativeUInt) > SizeOf(integer)) and (Size < SizeOf(NativeUInt)) then begin
        Li:= @Left;
        Ri:= @Right;

        for i:= 0 to (Size div SizeOf(integer)) - 1 do begin
          Same:= Li^ = Ri^;
          if not(Same) then goto DifferentInteger;
          Inc(Li);
          Inc(Ri);
        end;
        // Unaligned test for few remaining bytes
        NativeInt(Li):= NativeInt(Li) + (Size and (SizeOf(integer)- 1)) - SizeOf(integer);
        NativeInt(Ri):= NativeInt(Ri) + (Size and (SizeOf(integer)- 1)) - SizeOf(integer);
        if (Li^ = Ri^) then Result:= 0
        else begin
        DifferentInteger:
          SwapL.i:= Li^;
          SwapR.i:= Ri^;
          for i:= 0 to 3 do begin
            Result:= SwapL.Arr[i] - SwapR.Arr[i];
            if Result <> 0 then Exit;
          end;
        end;
      end else begin
        Lq:= @Left;
        Rq:= @Right;
        for i:= 0 to (Size div SizeOf(NativeUInt)) - 1 do begin
          Same:= Lq^ = Rq^;
          if not(Same) then goto DifferentNativeInt;
          Inc(Lq);
          Inc(Rq);
        end;
        // Unaligned test for few remaining bytes
        NativeUInt(Lq):= NativeUInt(Lq) + (Size and (SizeOf(NativeUInt)- 1)) - SizeOf(NativeUInt);
        NativeUInt(Rq):= NativeUInt(Rq) + (Size and (SizeOf(NativeUInt)- 1)) - SizeOf(NativeUInt);
        if (Lq^ = Rq^) then Result:= 0
        else begin
        DifferentNativeInt:
          SwapL.ni:= Lq^;
          SwapR.ni:= Rq^;
          for i:= 0 to SizeOf(NativeInt)- 1 do begin
            Result:= SwapL.Arr[i] - SwapR.Arr[i];
            if Result <> 0 then Exit;
          end;
        end;
      end; { else }
    end; { case else }
  end; { case }
end;
{$ELSE !PurePascal}
{$IFDEF CPUX64}
asm
    .NOFRAME
    cmp   RCX,RDX
    jz @Equal
    test  RCX,RDX
    jz @PossibleNilPointer
@NoNil:
    xchg  RSI,RAX
    // Left: RCX
    // Right: RDX
    // Size: R8   preserve:RBX, RBP, RDI, RSI, R12-R15
    mov   RSI, RCX
    xchg  RDI, RDX
    mov   RCX, R8
    repe  cmpsb
    xchg  RSI, RAX
    xchg  RDI, RDX
@Equal:
    seta  AL
    movzx EAX,AL
    sbb   EAX,0
    ret
@PossibleNilPointer:
    lea   R9, [RCX-1]
    lea   R10,[RDX-1]
    mov   R11,R10
    or    R11,R9
    jns @NoNil
    cmp   RCX,RDX
    seta  AL
    movzx EAX,AL
    sbb   EAX,0
end;
{$ENDIF}
{$IFDEF CPUX86}
asm
    // Left: EAX
    // Right: EDX
    // Size: ECX
    cmp   EAX,EDX
    jz @equal
    test  EAX,EDX
    jz @PossibleNilPointer
@NoNil:
    xchg  ESI,EAX
    xchg  EDI,EDX
    repe cmpsb
    xchg  ESI,EAX
    xchg  EDI,EDX
@equal:
    seta  AL
    movzx EAX,AL
    sbb   EAX,0
    ret
@PossibleNilPointer:
    or    EAX,EAX
    jnz @FirstParamNotNil
    dec   EAX
    ret
@FirstParamNotNil:
    or    EDX,EDX
    jnz @NoNil
    lea   EAX,[EDX+1]
    ret
end;
{$ENDIF}
{$ENDIF !PurePascal}

// Cannot be used for comparisons shorter than 4 bytes.
function BinaryEquals(const Left, Right: pointer; Size: integer): boolean;
{$IFDEF purepascal}
var
  i: integer;
  Lq, Rq: PNativeUInt;
  Li, Ri: PInteger;
begin
  if (SizeOf(NativeUInt) > SizeOf(integer)) and (Size < SizeOf(NativeUInt)) then begin
    Li:= Left;
    Ri:= Right;
    if (Li = Ri) then Exit(True)
    else if (Li = nil) or (Ri = nil) then Exit(False);
    for i:= 0 to (Size div SizeOf(integer)) - 1 do begin
      Result:= Li^ = Ri^;
      if not(Result) then Exit;
      Inc(Li);
      Inc(Ri);
    end;
    // Unaligned test for few remaining bytes
    NativeInt(Li):= NativeInt(Li) + (Size and (SizeOf(integer)- 1)) - SizeOf(integer);
    NativeInt(Ri):= NativeInt(Ri) + (Size and (SizeOf(integer)- 1)) - SizeOf(integer);
    Result:= Li^ = Ri^;
  end else begin
    Lq:= Left;
    Rq:= Right;
    if (Lq = Rq) then Exit(True)
    else if (Lq = nil) or (Rq = nil) then Exit(False);
    for i:= 0 to (Size div SizeOf(NativeUInt)) - 1 do begin
      Result:= Lq^ = Rq^;
      if not(Result) then Exit;
      Inc(Lq);
      Inc(Rq);
    end;
    // Unaligned test for few remaining bytes
    NativeUInt(Lq):= NativeUInt(Lq) + (Size and (SizeOf(NativeUInt)- 1)) - SizeOf(NativeUInt);
    NativeUInt(Rq):= NativeUInt(Rq) + (Size and (SizeOf(NativeUInt)- 1)) - SizeOf(NativeUInt);
    Result:= Lq^ = Rq^;
  end;
end;
{$ELSE !PurePascal}
{$IFDEF CPUX64}
// RCX: Left
// RDC: Right
// R8: size
asm
    .NOFRAME
    cmp   RCX,RDX
    jz @equal
    test  RCX,RDX
    jz @PossibleNilPointer
@NoNil:
    cmp   R8,4
    jns @MoreThan4bytes
    xor   RAX,RAX
    xor   R9,R9
    dec   R8
    jz @Compare1
    movzx EAX, word ptr [RCX+R8-1]
    movzx R9d, word ptr [RDX+R8-1]
    dec   R8
    jz @Confront
    shl   EAX,16
    shl   R9d,16
@compare1:
    mov   AL, [RCX]
    mov   R9b,[RDX]
@Confront:
    xor   EAX,R9d
    setz  AL
    ret
@MoreThan4Bytes:
    neg   R8
    // jz @equal
    sub   RCX, R8
    sub   RDX, R8
@loop8:
    add   R8,8
    jns @check4
    mov   RAX,[RCX+R8-8]
    xor   RAX,[RDX+R8-8]
    jz @loop8
@different:
    xor   EAX,EAX
    ret
@check4:
    sub   R8,4
    jg @smaller
    mov   EAX,[RCX+R8-4]
    xor   EAX,[RDX+R8-4]
    jnz @different
@smaller:
    and   R8,-4
    jz @equal
    mov   EAX,[RCX+R8]
    xor   EAX,[RDX+R8]
    setz  AL
    ret
@equal:
    mov   EAX,1
    ret
@PossibleNilPointer:
    lea   R9, [RCX-1]
    lea   R10,[RDX-1]
    mov   R11,R10
    or    R11,R9
    jns @NoNil
    xor   EAX,EAX
  end;
  {$ELSE !CPUX64}
  // EAX: Left
  // EDX: Right
  // ECX: size
  asm
    cmp   EAX,EDX
    jnz @Skip
    mov   AL,1
    ret
  @Skip:
    test  EAX,EDX
    jz @PossibleNilPointer
  @NoNil:
    push  EBX
    cmp   ECX,3
    jg @MoreThan3Bytes
    push  ESI
    dec   ECX
    jz @OneMoreByte
    movzx EBX, word ptr [EAX+ECX-1]
    movzx ESI, word ptr [EDX+ECX-1]
    dec   ECX
    xchg  ESI,ECX
    jz @DoneLoading
    shl   EBX,8
    shl   ECX,8
  @OneMoreByte:
    mov   BL, [EAX]
    mov   CL, [EDX]
  @DoneLoading:
    xor   EBX,ECX
    setz  AL
    pop   ESI
    pop   EBX
    ret
  @MoreThan3Bytes:
    neg   ECX
    // jz @equal
    sub   EAX, ECX
    sub   EDX, ECX
  @loop8:
    add   ECX,8
    jns @check4
    mov   EBX,[EAX+ECX-8]
    xor   EBX,[EDX+ECX-8]
    jnz @different
    mov   EBX,[EAX+ECX-8+4]
    xor   EBX,[EDX+ECX-8+4]
    jz @loop8
  @different:
    xor   EAX,EAX
    pop   EBX
    ret
  @check4:
    sub   ECX,4
    jg @smaller
    mov   EBX,[EAX+ECX-4]
    xor   EBX,[EDX+ECX-4]
    jnz @different
  @smaller:
    and   ECX,-4
    jz @equal
    mov   EBX,[EAX+ECX]
    xor   EBX,[EDX+ECX]
    jnz @different
  @equal:
    mov   AL,1
    pop   EBX
    ret
  @PossibleNilPointer:
    or    EAX,EAX
    jnz @FirstParamNotNil
    ret
  @FirstParamNotNil:
    or    EDX,EDX
    jnz @NoNil
    mov   EAX,EDX
end;
{$ENDIF}
{$ENDIF}

function FastBinaryCompare(const Left, Right; Size: integer): integer;
{$IFDEF purepascal}
var
  i: integer;
begin
  for i:= 0 to Size - 1 do begin
    Result:= PByte(Left)[i] - PByte(Right)[i];
    if Result <> 0 then Exit;
  end;
end;
{$ELSE !PurePascal}
{$IFDEF CPUX64}
asm
    .NOFRAME
    // Left: RCX
    // Right: RDX
    // Size: R8   preserve:RBX, RBP, RDI, RSI, R12-R15
    xor   RAX, RAX
    mov   R11, R8
    and   R11, $7
    xor   R8,  R11
    jz @loop2
    neg   R8
    sub   RCX, R8
    sub   RDX, R8
@loop:
    mov   RAX, [RCX+R8]
    bswap RAX
    mov   R9, [RDX+R8]
    bswap R9
    sub   RAX,R9
    jnz @different
    add   R8, 8
    jnz @loop
@loop2:
    dec   R11
    js @same
    movzx RAX, byte ptr [RCX+R8]
    movzx R9, byte ptr [RDX+R8]
    inc   R8
    sub   RAX,R9
    jz @loop2
@different:
    sbb   RAX,RAX
    sbb   RAX,-1
@same:
end;

{$ENDIF}
{$IFDEF CPUX86}
asm
    // //Left: EAX
    // //Right: EDX
    // //Size: ECX
    push  EBX
    push  EDI
    push  ESI
    xor   ESI, ESI  // In case Size = 0 and fallthrough to @same occurs.
    mov   EBX, ECX
    and   EBX, $3
    xor   ECX, EBX
    jz @loop2
    neg   ECX
    sub   EAX, ECX
    sub   EDX, ECX
  @loop:
    mov   ESI, [EAX+ECX]
    bswap ESI
    mov   EDI, [EDX+ECX]
    bswap EDI
    sub   ESI,EDI
    jnz @different
    add   ECX, 4
    jnz @loop
  @loop2:
    dec   EBX
    js @same
    movzx ESI, byte ptr [EAX+ECX]
    movzx EDI, byte ptr [EDX+ECX]
    inc   ECX
    sub   ESI,EDI
    jz @loop2
  @different:
    sbb   ESI,ESI
    sbb   ESI,-1
  @same:
    mov   EAX,ESI
    pop   ESI
    pop   EDI
    pop   EBX
end;
{$ENDIF CPUX86}
{$ENDIF !PurePascal}

function DynLen(const Arr: pointer): NativeInt; inline;
begin
  if Arr = nil then Result:= 0
  else Result:= PNativeInt(PByte(Arr) - SizeOf(NativeInt))^;
end;

function Compare_DynArray(const Left, Right: pointer; ElementSize: integer): NativeInt;
var
  LenL, LenR, lenDiff: NativeInt;
begin
  LenL:= DynLen(Left);
  LenR:= DynLen(Right);
  lenDiff:= LenL - LenR;
  LenL:= Min(LenL, LenR);
  Result:= FastBinaryCompare(Left^, Right^, ElementSize * LenL);
  if Result = 0 then Result:= lenDiff;
end;

function Compare_PSn(const Left, Right: OpenString): integer;
{$IFDEF purepascal}
begin
  Result:= (integer(Left > Right) - integer(Left < Right));
end;
{$ELSE}
{$IFDEF CPUX86}
begin
  Result:= (integer(Left > Right) - integer(Left < Right));
end;
{$ELSE}
asm
    { ->RCX = Pointer to left string }
    { RDX = Pointer to right string }
    XCHG   RBX,R8             // Push RBX
    MOV    R9,RCX
    MOV    R10,RDX

    // Are both S1 and S2 dword aligned?
    OR     ECX,EDX
    TEST   ECX,$3
    JZ @AlignedStrings
  @UnAlignedStrings:
    MOVZX  EAX, byte ptr [R9]  // Length S1
    MOVZX  ECX, byte ptr [R10] // Length S2
    INC    R9
    INC    R10
    //     ECX:= min(len1, len2)
    SUB    EAX,ECX { Diff = len1 - len2 }
    MOV    R11,RAX //save difference
    SBB    EBX,EBX //if len1 > len2 then EBX = 0 else EBX = -1
    AND    EAX,EBX //if len1 > len2 then Diff:= 0
    ADD    ECX,EAX //Diff:= Diff + len2  (a-b+b = a; 0+b=b
    MOV    RAX,R11
    NEG    RCX             // Count down
    SUB    R9, RCX
    SUB    R10,RCX
    CMP    RCX,-3
    JS @@LongLoop
  @ShortString:
    CMP    ECX,0
    JZ @@Exit
    CMP    ECX,-1
    JNZ @LengthNot1
    MOVZX  EDX, byte ptr [R9+ RCX]
    MOVZX  EBX, byte ptr [R10+RCX]
    JMP @@CompareRest
  @LengthNot1:
    CMP    ECX,-2
    JNZ @LengthIs3
    MOVZX  EDX, word ptr [R9+ RCX]
    MOVZX  EBX, word ptr [R10+RCX]
    JMP @@CompareRest
  @LengthIs3:
    MOV    EDX,[R9+ RCX-1]
    MOV    EBX,[R10+RCX-1]
    SHR    EDX,8
    SHR    EBX,8
    JMP @@CompareRest
  @AlignedStrings:
    MOV    EDX, [R9]
    MOV    EBX, [R10]

    MOVZX  EAX,DL
    MOVZX  ECX,BL
    SUB    EAX,ECX { EAX = len1 - len2 }
    JA  @@Skip1
    ADD    ECX,EAX { ECX = len2 + (len1 - len2) = len1 }
  @@Skip1:
    SHR    EBX,8
    SHR    EDX,8
    NEG    RCX
    // JZ @@Exit
    CMP    EBX,EDX
    JNE @@MisMatch
    ADD    RCX,3
    SUB    R9, RCX
    SUB    R10,RCX
    // MOV     R11,RCX
  @@longLoop:
    MOV    EDX,[R9 +RCX+4]
    MOV    EBX,[R10+RCX+4]
    CMP    EDX,EBX
    JNE @@misMatch
    ADD    RCX,4
    JNS @@exit
    MOV    EDX,[R9 +RCX+4]
    MOV    EBX,[R10+RCX+4]
    CMP    EDX,EBX
    JNE @@misMatch
    ADD    RCX,4
    JS     @@longloop
  @@exit:
    XCHG   RBX,R8
    RET
  @@compareRest:
    // RCX = 4/0 -> done
    // RCX = 3 -> 1 byte
    // RCX = 2 -> 2 bytes
    // RCX = 1 -> 3 bytes
    TEST   RCX,$3
    JZ  @@Exit
    LEA    ECX,[ECX*8]
    SHL    EDX,CL
    SHL    EBX,CL
    BSWAP  EDX
    BSWAP  EBX
    CMP    EDX,EBX
    JNE @@Different
    // if Same, then EAX is the difference in length
    XCHG   RBX,R8
    RET
  @@misMatch:
    ADD    RCX,4
    JG @@CompareRest
    BSWAP  EDX
    BSWAP  EBX
    CMP    EDX,EBX
  @@Different:
    SBB    EAX,EAX
    SBB    EAX,-1
    XCHG   RBX,R8
end;
{$ENDIF}{$ENDIF}

function CompareWideStr(const S1, S2: WideString): integer; overload;
// {$ifdef PurePascal}
var
  i: NativeInt;
  c1, c2: Char;
begin
  if pointer(S1) = pointer(S2) then Exit(0);
  if pointer(S1) = nil then Exit( -1);
  if pointer(S2) = nil then Exit(1);
  i:= low(S1);
  while (True) do begin
    c1:= S1[i];
    c2:= S2[i];
    Result:= integer(c1 > c2) - integer(c1 < c2);
    if (integer(Result = 0) and integer(c1 <> #0) and integer(c2 <> #0)) = 0 then Exit;
    Inc(i);
  end;
end;
// {$else !PurePascal}
// {$ifdef CPUX64}
// asm
//
// end;
// {$else !CPUX64}
// asm
//
// end;
// {$endif}{$endif}

function SameWideStr(const S1, S2: WideString): boolean; overload;
{$IFDEF PurePascal}
const
  Forever = False;
var
  i: integer;
begin
  Result:= pointer(S1) = pointer(S2);
  if Result then Exit;
  Result:= not((pointer(S1) = nil) or (pointer(S2) = nil));
  if not(Result) then Exit;
  for i:= low(S1) to high(integer) do begin
    Result:= S1[i] = S2[i];
    if not(Result) or (S1[i] = #0) then Exit;
  end;
end;
{$ELSE}
{$IFDEF CPUX64}
asm
    .NOFRAME
    cmp   RCX, RDX
    jz @done       // pointers are the same
    test  RCX, RDX
    jz @PossibleNilPointer
@StartLoop:
    xor  	R10, R10
@loop:
    movzx	RAX, word ptr [RCX + R10]	// fetch bytes
    movzx	R9, word ptr [RDX + R10]

    test	EAX, R9d		// if there's a null char
    jz	@dif
    sub	  EAX, R9d		// compare bytes,
    jne	@done		// quit if not equal

    add   R10,2		// else go for next round
    jmp	@loop
@dif:
    sub   EAX,R9d
@done:
    setz  AL
    movzx EAX, AL
    ret
@PossibleNilPointer:
    or    RCX, RCX
    jz @NotEqual
    or    RDX, RDX
    jnz @StartLoop
@NotEqual:
    xor   EAX,EAX
    ret
end;
{$ELSE !CPUX64}
asm
    push  ESI
    push  EDI
    cmp  EAX,EDX
    jz @done
    test EAX, EDX
    jnz @NoNilPointer
    or   EAX,EAX
    jz @dif
    or   EDX,EDX
    jz @dif
  @NoNilPointer:
    xor  	ECX, ECX
  @loop:
    movzx	ESI, word ptr [EAX + ECX]	// fetch bytes
    movzx	EDI, word ptr [EDX + ECX]

    cmp	  EDI, ESI		// compare bytes,
    jne	@done	      	// quit if not equal
    test	ESI, EDI		// if there's a null char
    jz	@done

    add   ECX,2		// else go for next round
    jmp	@loop
  @dif:
    sub   EDI,ESI
  @done:
    setz  AL
    pop   ESI
    pop   EDI
    ret
end;
{$ENDIF}{$ENDIF}

function SameUnicodeStr(const S1, S2: string): boolean;
{$IFDEF PurePascal} inline;
begin
  Result:= System.SysUtils.SameStr(S1, S2);
end;
{$ELSE !PurePascal}
{$IFDEF CPUX64}
// RCX = S1
// RDX = S2
// Because the string is zero-terminated it is always safe to read 2 bytes beyond the last char.
asm
    .NOFRAME
    cmp   RCX, RDX
    je @done
    sub   RCX,4
    js @done  // S1 is nil, S2 is not
    sub   RDX,4
    js @done  // S2 is nil, S2 is not
@NoNilStrings:
    // Compare the length and 2st two chars
    mov   R8,[RCX]
    mov   R9,[RDX]
    cmp   R9,R8
    jnz @done  // The lengths are different
    mov   R8d,R8d   // zero-out non length parts.
    add   R8,R8     // WideChar = 2 bytes.
    neg   R8
    // use negative based indexing
    sub   RCX, R8
    sub   RDX, R8
    add   R8,4 // First 2 chars are already done
    jz @Equal
    mov   EAX,[RCX+R8+4]   // Realign the qword reads
    xor   EAX,[RDX+R8+4]
    jnz @done
@compareLoop:
    add   R8,8
    jns @DoTail8
    mov   RAX,[RCX+R8]
    xor   RAX,[RDX+R8]
    jz @compareLoop
    xor   RAX,RAX  // Not equal
    ret
@DoTail8:
    // mov  R8,-4
    mov   RAX,[RCX+4-8]
    xor   RAX,[RDX+4-8]
    setz  AL
    ret
@done:
    setz  AL
    ret
@equal:
    mov   AL,1
end;
{$ELSE !CPUX64}
// EAX = S1
// EDX = S2
asm
    push  EBX
    cmp   EAX, EDX
    je @equal
    sub   EAX,4
    js @different  // S1 is nil, S2 is not
    sub   EDX,4
    js @different  // S2 is nil, S2 is not
@NoNilStrings:
    // Compare the length and 2st two chars
    mov   ECX,[EAX]
    cmp   ECX,[EDX]
    jnz @different  // The lengths are different
    mov   EBX,[EAX+4]
    xor   EBX,[EDX+4]
    jnz @different
    add   ECX, ECX
    neg   ECX
    // use negative based indexing
    sub   EAX, ECX
    sub   EDX, ECX
@compareLoop:
    add   ECX,4 // First 2 chars are already done
    jge @Equal // Strings are equal.
    mov   EBX,[EAX+ECX+4]
    xor   EBX,[EDX+ECX+4]
    jz @compareLoop
@different:
    xor   EAX,EAX
    pop   EBX
    ret
@equal:
    mov   AL,1
    pop   EBX
end;
{$ENDIF CPUX64}{$ENDIF PurePascal}

function CompareAnsiStr(const S1, S2: AnsiString): integer;
{$IFDEF PUREPASCAL}
var
  P1, P2: PAnsiChar;
  i: integer;
  L1, L2: integer;
begin
  { Length and PChar of S1 }
  L1:= Length(S1);
  P1:= PAnsiChar(S1);

  { Length and PChar of S2 }
  L2:= Length(S2);
  P2:= PAnsiChar(S2);

  { Continue the loop until the end of one string is reached. }
  i:= 0;
  while (i < L1) and (i < L2) do begin
    if (P1^ <> P2^) then Exit(Ord(P1^) - Ord(P2^));

    Inc(P1);
    Inc(P2);
    Inc(i);
  end;

  { If chars were not different return the difference in length }
  Result:= L1 - L2;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
inline;
begin
  Result:= System.AnsiStrings.CompareStr(S1, S2);
end;
{$ELSE}
asm // StackAligned
    { On entry:
    rCx = @S1[1]
    RDX = @S2[1]
    //preserve:RBX, RBP, RDI, RSI, R12-R15
    On exit:
    Result in EAX:
    0 if S1 = S2,
    > 0 if S1 > S2,
    < 0 if S1 < S2
    Code size: ??? bytes }
    { On entry:
    EAX = @S1[1]
    EDX = @S2[1]
    On exit:
    Result in EAX:
    0 if S1 = S2,
    > 0 if S1 > S2,
    < 0 if S1 < S2
    Code size:
    101 bytes }
    cmp   RCX, RDX
    je @SameAnsiString
    { Is either of the AnsiStrings perhaps nil? }
    test  RCX, RDX
    jz @PossibleNilAnsiString
    { Compare the first character (there has to be a trailing #0). In random
    AnsiString compares this can save a lot of CPU time. }
@BothNonNil:
    { Compare the first character }
    movzx EAX, byte ptr [RCX]
    movzx R8d, byte ptr [RDX]
    sub   EAX,R8d
    je @FirstCharacterSame
    { First character differs }
    ret
@FirstCharacterSame:
    { Save EBX }
    push  RBX
    { Set EBX = length(S1) }
    mov   EBX, [RCX - 4]
    mov   R9d,EBX
    xor   R8, R8
    { Set EBX = length(S1) - length(S2) }
    sub   EBX, [RDX - 4]
    { Save the length difference in R11 }
    mov   R11,RBX
    { Set ECX = 0 if length(S1) < length(S2), $ffffffff otherwise }
    adc   R8, -1
    { Set ECX = - min(length(S1), length(S2)) }
    and   R8d, EBX
    sub   R8d, R9d
    movsx R8,R8d
    { Adjust the pointers to be negative based }
    sub   RCX, R8
    sub   RDX, R8
@CompareLoop:
    mov   EBX, [RCX + R8]
    xor   EBX, [RDX + R8]
    jnz @Mismatch
    add   R8, 4
    js @CompareLoop
    { All characters match - return the difference in length }
@MatchUpToLength:
    mov   RAX,R11
    pop   RBX
    ret
@Mismatch:
    bsf   EBX, EBX
    shr   EBX, 3
    add   R8, RBX
    jns @MatchUpToLength
    movzx EAX, byte ptr [RCX + R8]
    movzx EDX, byte ptr [RDX + R8]
    sub   EAX, EDX
    pop   RBX
    ret
    { It is the same AnsiString }
@SameAnsiString:
    xor   EAX, EAX
    ret
    { Good possibility that at least one of the AnsiStrings are nil }
@PossibleNilAnsiString:
    test  RCX, RCX
    jz @FirstAnsiStringNil
    test  RDX, RDX
    jnz @BothNonNil
    { Return first AnsiString length: second AnsiString is nil }
    mov   EAX, 1
    ret
@FirstAnsiStringNil:
    { Return 0 - length(S2): first AnsiString is nil }
    mov   EAX,-1
end;
{$ENDIF X86ASM}
{$ENDIF !PUREPASCAL}

function CompareUnicodeStr(const S1, S2: string): integer;
{$IFDEF PurePascal} inline;
begin
  Result:= System.SysUtils.CompareStr(S1, S2);
end;
{$ELSE !PurePascal}
{$IFDEF CPUX64}
asm // StackAligned
    { On entry:
    RCX = @S1[1]
    RDX = @S2[1]
    //preserve:RBX, RBP, RDI, RSI, R12-R15
    On exit:
    Result in EAX:
    0 if S1 = S2,
    > 0 if S1 > S2,
    < 0 if S1 < S2
    Code size: ??? bytes }
    .NOFRAME
    CMP   RCX, RDX
    JE @SameString
    { Is either of the strings perhaps nil? }
    TEST  RCX, RDX
    JNZ @BothNonNil
    OR    RCX,RCX
    JNZ @TestStr2
    MOV   EAX,-1
    RET
  @TestStr2:
    OR    RDX,RDX
    JNZ @BothNonNil
    MOV   EAX,1
    RET
    { Compare the first two characters (there has to be a trailing #0). In random
    string compares this can save a lot of CPU time. }
  @BothNonNil:
    { Compare the first two characters }
    MOV   EAX, DWORD PTR [RCX]
    MOV   R8d, DWORD PTR [RDX]
    CMP   EAX, R8d
    JE @FirstTwoCharacterSame
    { First two characters differ }
    { Swap 2 and 1 char to the correct position }
    ROL   EAX,16
    ROL   R8d,16
    SUB   EAX, R8d
    RET
  @FirstTwoCharacterSame:
    { Save EBX }
    PUSH  RBX
    { Set EBX = length(S1) }
    MOV   EBX, [RCX - 4]
    MOV   R10,RBX
    XOR   R8, R8
    { Set EBX = length(S1) - length(S2) }
    MOV   R9d,[RDX - 4]
    SUB   RBX, R9
    { Save the length difference }
    MOV   R9,RBX
    { Set ECX = 0 if length(S1) < length(S2), $ffffffff otherwise }
    ADC   R8, -1
    { Set ECX = - min(length(S1), length(S2)) }
    AND   R8, RBX
    SUB   R8, R10
    SAL   R8, 1
    { Adjust the pointers to be negative based }
    SUB   RCX, R8
    SUB   RDX, R8
  @CompareLoop:
    MOV   EBX, [RCX + R8]
    XOR   EBX, [RDX + R8]
    JNZ @Mismatch
    ADD   R8, 4
    JS @CompareLoop
    { All characters match - return the difference in length }
  @MatchUpToLength:
    MOV   RAX,R9
    POP   RBX
  @Done:
    RET
  @Mismatch:
    // BSF EBX, EBX      //where is differing bit?
    // SHR EBX, 4        //in first or second word?
    // ADD EBX, EBX
    { proposal: }           // BSF is an expensive instruction
    AND   EBX, $FFFF
    SETZ  BL
    ADD   EBX,EBX
    { end proposal }
    ADD   R8, RBX
    JNS @MatchUpToLength
    MOVZX EAX, WORD PTR [RCX + R8]
    MOVZX EDX, WORD PTR [RDX + R8]
    SUB   EAX, EDX
    POP   RBX
    RET
    { It is the same string }
  @SameString:
    XOR   EAX, EAX
    RET
    { Good possibility that at least one of the strings are nil }
  @PossibleNilString:
    TEST  RCX, RCX
    JZ @FirstStringNil
    TEST  RDX, RDX
    JNZ @BothNonNil
    { Return first string length: second string is nil }
    // MOV EAX, [RCX - 4]
    MOV   EAX,1
    RET
@FirstStringNil:
    { Return 0 - length(S2): first string is nil }
    // SUB EAX, [RDX - 4]
    DEC   EAX
end;
{$ELSE !CPUX64} inline;
begin
  Result:= System.SysUtils.CompareStr(S1, S2);
end;
{$ENDIF CPUX64}{$ENDIF PurePascal}

function CompareFast(const Left, Right: pointer): integer;
begin
  Result:= (integer(NativeUInt(Left) > NativeUInt(Right)) -
    integer(NativeUInt(Left) < NativeUInt(Right)));
end;

function CompareFast(const Left, Right: UTF8String): integer;
begin
  Result:= CompareFast(AnsiString(Left), AnsiString(Right));
end;

function CompareFast(const Left, Right: RawByteString): integer;
begin
  Result:= CompareFast(AnsiString(Left), AnsiString(Right));
end;

function CompareFast(const Left, Right: WideString): integer;
begin
  Result:= CompareWideStr(Left, Right);
end;

function CompareFast(const Left, Right: UnicodeString): integer;
begin
  Result:= CompareUnicodeStr(Left, Right);
end;

function CompareFast(const Left, Right: AnsiString): integer;
begin
  Result:= CompareStr(Left, Right);
end;

function CompareFast(const Left, Right: ShortString): integer;
begin
  Result:= CompareStr(System.ShortString(Left), System.ShortString(Right));
end;

function CompareFast(const Left, Right: bytebool): integer;
begin
  Result:= integer(byte(Left) <> 0) - integer(byte(Right) <> 0);
end;

function CompareFast(const Left, Right: longbool): integer;
begin
  Result:= integer(integer(Left) <> 0) - integer(integer(Right) <> 0);
end;

function CompareFast(const Left, Right: wordbool): integer;
begin
  Result:= integer(integer(Left) <> 0) - integer(integer(Right) <> 0);
end;

function CompareFast(const Left, Right: boolean): integer;
begin
  Result:= integer(byte(Left) <> 0) - integer(byte(Right) <> 0);
end;

function CompareFast(const Left, Right: Currency): integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: Comp): integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: extended): integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: double): integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: Real48): integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: single): integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: UCS4Char): integer;
begin
  Result:= CompareFast(cardinal(Left), cardinal(Right));
end;

function CompareFast(const Left, Right: WideChar): integer;
begin
  Result:= CompareFast(word(Left), word(Right));
end;

function CompareFast(const Left, Right: AnsiChar): integer;
begin
  Result:= CompareFast(byte(Left), byte(Right));
end;

function CompareFast(const Left, Right: NativeUInt): integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: NativeInt): integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: Int64): integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: UInt64): integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: integer): integer;
begin
  //Result:= Left - Right;
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: cardinal): integer;
begin
  Result:= integer(Left > Right) - integer(Left < Right);
end;

function CompareFast(const Left, Right: int16): integer;
begin
  Result:= integer(Left) - integer(Right);
end;

function CompareFast(const Left, Right: word): integer;
begin
  Result:= integer(Left) - integer(Right);
end;

function CompareFast(const Left, Right: int8): integer;
begin
  Result:= integer(Left) - integer(Right);
end;

function CompareFast(const Left, Right: byte): integer;
begin
  Result:= integer(Left) - integer(Right);
end;

function CompareFast(const Left, Right: TObject): integer;
begin
  Result:= CompareFast(NativeUInt(Left), NativeUInt(Right));
end;

function CompareFast(const Left, Right: IInterface): integer;
begin
  Result:= CompareFast(NativeUInt(Left), NativeUInt(Right));
end;

end.
