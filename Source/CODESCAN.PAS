unit codescan;
{ Bookland *  Code 39 (Mod 43) * Code 128 (Automatic) *
  EAN 8 Plus 2 * EAN 8 Plus 5 * EAN 13 Plus 2 * EAN 13 Plus 5 *
  EAN 128 * Interleaved 2-of-5 (Mod 10) * ISBN * ISSN *
  POSTNET * UPC A Plus 2 * UPC A Plus 5 * UPC E Plus 2 * UPC E Plus 5 *
  UCC / EAN 128 * SCC-14 (UPC Shipping Container Code) *
  SSCC-18 (Serial Shipping Container Code)
}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Funcs, Math, DelphiZXingQRCode, QR_win1251;


const
  bc_TypeCodeCount = 13;
  bc_None  = -1;
  bc_QR    =  0;  // QR
  bc_EAN8  =  1;  // EAN 8
  bc_EAN13 =  2;  // EAN 13
  bc_UPCA  =  3;  // UPC A
  bc_UPCE  =  4;  // UPC E
  bc_25    =  5;  // Interleaved 2-of-5
  bc_39    =  6;  // Code 39
  bc_93    =  7;  // Code 93
  bc_93A   =  8;  // Code 93 (Extended)
  bc_128A  =  9;  // Code 128 (A)
  bc_128B  = 10;  // Code 128 (B)
  bc_128C  = 11;  // Code 128 (C)
  bc_BAR   = 12;  // Codabar
  bc_Auto  = 255; // Надо определить подходящий тип

  bc_CodeName      : array[-1..bc_TypeCodeCount-1] of String[18] =
  ('','QR-code ','EAN 8','EAN 13','UPC A','UPC E','Interleaved 2 of 5','Code 39','Code 93',
   'Code 93 ASCII','Code 128 A','Code 128 B','Code 128 C','Codebar');

  bc_CodeSize      : array[-1..bc_TypeCodeCount-1] of Integer =
  ( 0, 0, 8,13,12, 7, 0, 0, 0, 0, 0, 0, 0, 0);

  bc_CodeControl   : array[-1..bc_TypeCodeCount-1] of Integer =
  ( 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0);

  bc_CodeChar      : array[-1..bc_TypeCodeCount-1] of String =
  ( '',
 {0} '',
 {1} '0123456789',
 {2} '0123456789',
 {3} '0123456789',
 {4} '0123456789',
 {5} '0123456789-$:/.+',
 {6} '0123456789 %$.-+/*ABCDEFGHIJKLMNOPQRSTUVWXYZ',
 {7} '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%',
 {8}  #00#01#02#03#04#05#06#07#08#09#10#11#12#13#14#15#16#17#18#19#20#21#22#23+
      #24#25#26#27#28#29#30#31+
     ' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_'+
     '`abcdefghijklmnopqrstuvwxyz{|}~'+#127,
 {9} ' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_',
{10} ' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_'
                                     +'`abcdefghijklmnopqrstuvwxyz{|}~',
{11} '0123456789',
{12} '0123456789-$:/.+');

 ParitetEAN : array[0..9,1..6]of Byte=
  ((0,0,0,0,0,0), (0,0,1,0,1,1), (0,0,1,1,0,1), (0,0,1,1,1,0), (0,1,0,0,1,1),
   (0,1,1,0,0,1), (0,1,1,1,0,0), (0,1,0,1,0,1), (0,1,0,1,1,0), (0,1,1,0,1,0));

 ParitetUPC : array[0..9,1..6]of Byte=
  ((1,1,1,0,0,0), (1,1,0,1,0,0), (1,1,0,0,1,0), (1,1,0,0,0,1), (1,0,1,1,0,0),
   (1,0,0,1,1,0), (1,0,0,0,1,1), (1,0,1,0,1,0), (1,0,1,0,0,1), (1,0,0,1,0,1));

 ParitetADD : array[0..3,1..2]of Byte = ((0,0),(0,1),(1,0),(1,1));

 EAN : array[0..9,0..3] of AnsiString =
 (( '0002202', '0200222', '2022000', '2220020' ),
  ( '0022002', '0220022', '2002200', '2200220' ),
  ( '0020022', '0022022', '2200200', '2202200' ),
  ( '0222202', '0200002', '2022220', '2000020' ),
  ( '0200022', '0022202', '2200020', '2022200' ),
  ( '0220002', '0222002', '2000220', '2002220' ),
  ( '0202222', '0000202', '2222020', '2020000' ),
  ( '0222022', '0020002', '2202220', '2000200' ),
  ( '0220222', '0002002', '2220220', '2002000' ),
  ( '0002022', '0020222', '2202000', '2220200' ));

 CBar : Array[-1..19] of AnsiString =
  (              '',  {0}'101010001110',  {1}'101011100010',  {2}'101000101110',
  {3}'111000101010',  {4}'101110100010',  {5}'111010100010',  {6}'100010101110',
  {7}'100010111010',  {8}'100011101010',  {9}'111010001010',  {-}'101000111010',
 { $}'101110001010',  {:}'11101011101110',{/}'11101110101110',{.}'11101110111010',
  {+}'10111011101110',{A}'10111000100010',{B}'10001000101110',{C}'10100010001110',
  {D}'10100011100010');

 C25 : array[-1..15] of AnsiString =
  (            '',    {0}'1010100110',    {1}'1010110010',    {2}'1010010110',
  {3}'1100101010',    {4}'1011010010',    {5}'1101010010',    {6}'1001010110',
  {7}'1001011010',    {8}'1001101010',    {9}'1101001010',    {-}'1010011010',
 { $}'1011001010',    {:}'11010110110',   {/}'11011010110',   {.}'11011011010',
  {+}'10110110110');

 C39 : array[-1..43] of AnsiString =
  (               '', {0}'1010011011010', {1}'1101001010110', {2}'1011001010110',
  {3}'1101100101010', {4}'1010011010110', {5}'1101001101010', {6}'1011001101010',
  {7}'1010010110110', {8}'1101001011010', {9}'1011001011010', { }'1001101011010',
  {%}'1010010010010',{ $}'1001001001010', {.}'1100101011010', {-}'1001010110110',
  {+}'1001010010010', {/}'1001001010010', {*}'1001011011010', {A}'1101010010110',
  {B}'1011010010110', {C}'1101101001010', {D}'1010110010110', {E}'1101011001010',
  {F}'1011011001010', {G}'1010100110110', {H}'1101010011010', {I}'1011010011010',
  {J}'1010110011010', {K}'1101010100110', {L}'1011010100110', {M}'1101101010010',
  {N}'1010110100110', {O}'1101011010010', {P}'1011011010010', {Q}'1010101100110',
  {R}'1101010110010', {S}'1011010110010', {T}'1010110110010', {U}'1100101010110',
  {V}'1001101010110', {W}'1100110101010', {X}'1001011010110', {Y}'1100101101010',
  {Z}'1001101101010');

 C93 : array[-1..48] of AnsiString =
  (           '',     {0}'100010100',     {1}'101001000',     {2}'101000100',
  {3}'101000010',     {4}'100101000',     {5}'100100100',     {6}'100100010',
  {7}'101010000',     {8}'100010010',     {9}'100001010',     {A}'110101000',
  {B}'110100100',     {C}'110100010',     {D}'110010100',     {E}'110010010',
  {F}'110001010',     {G}'101101000',     {H}'101100100',     {I}'101100010',
  {J}'100110100',     {K}'100011010',     {L}'101011000',     {M}'101001100',
  {N}'101000110',     {O}'100101100',     {P}'100010110',     {Q}'110110100',
  {R}'110110010',     {S}'110101100',     {T}'110100110',     {U}'110010110',
  {V}'110011010',     {W}'101101100',     {X}'101100110',     {Y}'100110110',
  {Z}'100111010',     {-}'100101110',     {.}'111010100',     { }'111010010',
 { $}'111001010',     {/}'101101110',     {+}'101110110',     {%}'110101110',
 {S1}'100100110',    {S2}'111011010',    {S3}'111010110',    {S4}'100110010',
                 {START }'101011110',                    {STOP  }'1010111101');

 C93A : array[-1..127] of AnsiString = ('',
 {0 NUL}'2U', {1 SOH}'1A', {2 STX}'1B', {3 ETX}'1C', {4 EOT}'1D', {5 ENQ}'1E',
 {6 ACK}'1F', {7 BEL}'1G', {8 BS }'1H', {9 HT }'1I', {10LF }'1J', {11VT }'1K',
 {12FF }'1L', {13CR }'1M', {14SO }'1N', {15SI }'1O', {16DLE}'1P', {17DC1}'1Q',
 {18DC2}'1R', {19DC3}'1S', {20DC4}'1T', {21NAK}'1U', {22SYN}'1V', {23ETB}'1W',
 {24CAN}'1X', {25EM }'1Y', {26SUB}'1Z', {27ESC}'2A', {28FS }'2B', {29GS }'2C',
 {30RS }'2D', {31US }'2E', {32   } ' ', {33 ! }'3A', {34 “ }'3B', {35 # }'3C',
 {36 $ } '$', {37 % } '%', {38 & }'3F', {39 ' }'3G', {40 ( }'3H', {41 ) }'3I',
 {42 * }'3J', {43 + } '+', {44 , }'3L', {45 - } '-', {46 . } '.', {47 / } '/',
 {48 0 } '0', {49 1 } '1', {50 2 } '2', {51 3 } '3', {52 4 } '4', {53 5 } '5',
 {54 6 } '6', {55 7 } '7', {56 8 } '8', {57 9 } '9', {58 : }'3Z', {59 ; }'2F',
 {60 < }'2G', {61 = }'2H', {62 > }'2I', {63 ? }'2J', {64 @ }'2V', {65 A } 'A',
 {66 B } 'B', {67 C } 'C', {68 D } 'D', {69 E } 'E', {70 F } 'F', {71 G } 'G',
 {72 H } 'H', {73 I } 'I', {74 J } 'J', {75 K } 'K', {76 L } 'L', {77 M } 'M',
 {78 N } 'N', {79 O } 'O', {80 P } 'P', {81 Q } 'Q', {82 R } 'R', {83 S } 'S',
 {84 T } 'T', {85 U } 'U', {86 V } 'V', {87 W } 'W', {88 X } 'X', {89 Y } 'Y',
 {90 Z } 'Z', {91 [ }'2K', {92 \ }'2L', {93 ] }'2M', {94 ^ }'2N', {95 _ }'2O',
 {96 ` }'2W', {97 a }'4A', {98 b }'4B', {99 c }'4C', {10 d }'4D', {01 e }'4E',
 {02 f }'4F', {03 g }'4G', {10 h }'4H', {05 i }'4I', {06 j }'4J', {07 k }'4K',
 {10 l }'4L', {09 m }'4M', {10 n }'4N', {11 o }'4O', {11 p }'4P', {13 q }'4Q',
 {14 r }'4R', {15 s }'4S', {11 t }'4T', {17 u }'4U', {18 v }'4V', {19 w }'4W',
 {12 x }'4X', {21 y }'4Y', {22 z }'4Z', {23 { }'2P', {12 | }'2Q', {25 }  '2R',
 {26 ~ }'2S', {27DEL}'2T'  );

 C128: array[-1..106]of AnsiString = ('',
  { }'11011001100',   {!}'11001101100',   {"}'11001100110',   {#}'10010011000',
 { $}'10010001100',   {%}'10001001100',   {&}'10011001000',   {'}'10011000100',
  {(}'10001100100',   {)}'11001001000',   {*}'11001000100',   {+}'11000100100',
  {,}'10110011100',   {-}'10011011100',   {.}'10011001110',   {/}'10111001100',
  {0}'10011101100',   {1}'10011100110',   {2}'11001110010',   {3}'11001011100',
  {4}'11001001110',   {5}'11011100100',   {6}'11001110100',   {7}'11101101110',
  {8}'11101001100',   {9}'11100101100',   {:}'11100100110',   {;}'11101100100',
  {<}'11100110100',   {=}'11100110010',   {>}'11011011000',   {?}'11011000110',
  {@}'11000110110',   {A}'10100011000',   {B}'10001011000',   {C}'10001000110',
  {D}'10110001000',   {E}'10001101000',   {F}'10001100010',   {G}'11010001000',
  {H}'11000101000',   {I}'11000100010',   {J}'10110111000',   {K}'10110001110',
  {L}'10001101110',   {M}'10111011000',   {N}'10111000110',   {O}'10001110110',
  {P}'11101110110',   {Q}'11010001110',   {R}'11000101110',   {S}'11011101000',
  {T}'11011100010',   {U}'11011101110',   {V}'11101011000',   {W}'11101000110',
  {X}'11100010110',   {Y}'11101101000',   {Z}'11101100010',   {[}'11100011010',
  {\}'11101111010',   {]}'11001000010',   {^}'11110001010',   {_}'10100110000',
  {`}'10100001100',   {a}'10010110000',   {b}'10010000110',   {c}'10000101100',
  {d}'10000100110',   {e}'10110010000',   {f}'10110000100',   {g}'10011010000',
  {h}'10011000010',   {i}'10000110100',   {j}'10000110010',   {k}'11000010010',
  {l}'11001010000',   {m}'11110111010',   {n}'11000010100',   {o}'10001111010',
  {p}'10100111100',   {q}'10010111100',   {r}'10010011110',   {s}'10111100100',
  {t}'10011110100',   {u}'10011110010',   {v}'11110100100',   {w}'11110010100',
  {x}'11110010010',   {y}'11011011110',   {z}'11011110110',   {{}'11110110110',
  {|}'10101111000',   {} '10100011110',   {~}'10001011110', {DEL}'10111101000',
{FN3}'10111100010', {FN2}'11110101000',{SHFT}'11110100010',{CodC}'10111011110',
{FN4}'10111101110',{CodA}'11101011110', {FN1}'11110101110', {<A>}'11010000100',
{<B>}'11010010000', {<C>}'11010011100',{Stop}'1100011101011'  );

Function bc_GetControlChar(codeType:Integer; s:string):String;
Function bc_IsCodeType(CodeType:Integer; Code: String; var s: String): Boolean;

//Function bc_GetLineCode(CodeType:Integer; _code:string; var _line:string):boolean;
//Function bc_PaintLineCode(TC:TCanvas; CodeType:Integer; code: string; QuietZone: Integer = -1):boolean;

Type
  TBarCode = class
    private
      FQRCode: TDelphiZXingQRCode;

      FCodeType: Integer;
      FCode: String;
      FCheckCode: String;
      FLineCode: string;
    public
      constructor Create;
      destructor Destroy; override;

      function SetCode(aCodeType: Integer; aCode: String): Boolean;
      function CheckCode: Boolean;
      function Width: Integer;
      procedure PaintBarCode(TC:TCanvas; QuietZone: Integer = -1);
      procedure SaveToFile(aFileName: String; PixelSize: Integer = 32);

      property CodeType: Integer read FCodeType;
      property Code: String read FCode;
  end;

implementation

//==============================================================================
Function bc_GetControlChar(codeType:Integer; s:string):String;
var n,i : Integer;
begin
  result:= EmptyStr;

  if CodeType in [bc_EAN8, bc_EAN13, bc_UPCA, bc_UPCE]
  then begin While Length(s)<bc_CodeSize[CodeType]-bc_CodeControl[CodeType]
              do s:='0'+s;
             //.................................................................
             if CodeType=bc_UPCE
             then case s[6] of
                   '0','1','2': s:='0'+s[1]+s[2]+s[6]+'0000' +s[3]+s[4]+s[5];
                           '3': s:='0'+s[1]+s[2]+s[3]+'00000'+s[4]+s[5];
                           '4': s:='0'+s[1]+s[2]+s[3]+s[4]+'00000'+s[5]
                   else         s:='0'+s[1]+s[2]+s[3]+s[4]+s[5]+'0000'+s[6];
                  end;
             //.................................................................
             n:=0;
             if Not isDigit(s) then Exit;
             if Odd(Length(s)) then s:='0'+s;

             for i:=1 to Length(s) do
               if odd(i) then n:=n+  StrToInt(s[i])
                         else n:=n+3*StrToInt(s[i]);

             n:=(30000-n) mod 10;
             result:=Chr(n+48);
       end;
end;
//------------------------------------------------------------------------------
Function bc_IsCodeType(CodeType:Integer; Code: String; var s: String): Boolean;
var i     : Integer;
begin
  if CodeType = bc_QR then Exit(true); // QR-code
  
  s:=Code;
  If Length(Code)=0 then Exit(CodeType=bc_None);

  Result:=True;
  for i:=1 to Length(Code) do
    Result:=(Result) and (0<Pos(Code[i],bc_CodeChar[CodeType]));
  If Not Result then Exit;

  If bc_CodeControl[CodeType]=0 Then Exit;

  While Length(s) < bc_CodeSize[CodeType] do s:='0'+s;

  if (0<bc_CodeSize[CodeType])and(Length(s)>bc_CodeSize[CodeType])
  then Result:=False
  else Result:=(bc_GetControlChar( CodeType, Copy(s,1,Length(s)  -bc_CodeControl[CodeType]))
                                            =Copy(s,  Length(s)+1-bc_CodeControl[CodeType],bc_CodeControl[CodeType]));
end;
//------------------------------------------------------------------------------
Function bc_GetLineCode(CodeType:Integer; _code:string; var _line:string):boolean;
var s,t          : AnsiString;
    i,P,Key,Key2 : Integer;
    List         : TList;
begin
  Result :=False;

  _line:= EmptyStr;
  if Not (CodeType in [0..bc_TypeCodeCount-1]) then exit;

  try
    case CodeType of
      bc_EAN8:  begin t:='101';
                      for i:=1 to  4 do t:=t+EAN[StrToInt(_code[i]),0];
                      t:=t+'01010';
                      for i:=5 to  8 do t:=t+EAN[StrToInt(_code[i]),3];
                      t:=t+'101';
                end;
      bc_EAN13: begin t:='101';
                      for i:=2 to  7 do t:=t+EAN[StrToInt(_code[i]), ParitetEAN[StrToInt(_code[1]),i-1]];
                      t:=t+'01010';
                      for i:=8 to 13 do t:=t+EAN[StrToInt(_code[i]),3];
                      t:=t+'101';
                 end;
      bc_UPCA:  begin t:='101';
                      for i:=1 to  6 do t:=t+EAN[StrToInt(_code[i]),0];
                      t:=t+'01010';
                      for i:=7 to 12 do t:=t+EAN[StrToInt(_code[i]),3];
                      t:=t+'101';
                end;
      bc_UPCE:  begin t:='101';
                      for i:=1 to 6 do t:=t+EAN[StrToInt(_code[i]), ParitetUPC[StrToInt(_code[7]),i]];
                      t:=t+'010101';
                end;
      bc_25:    begin t:='10100110010';
                      for i:=1 to Length(_code) do
                      begin
                        P:=Pos(_code[i],bc_CodeChar[CodeType])-1;
                        t:=t+C25[P];
                      end;
                      t:=t+'10100110010';
                end;
      bc_39:    begin t:='01001011011010';
                      for i:=1 to Length(_code) do
                      begin
                        P:=Pos(UPPERCASE(_code[i]),bc_CodeChar[CodeType])-1;
                        t:=t+C39[P];
                      end;
                      t:=t+'01001011011010';
                end;
     bc_93,bc_93A:
                begin List:=TList.Create;
                      for i:=1 to Length(_code) do
                      begin
                        t:=C93A[ORD(_Code[i])];
                        if Length(t)=2 then List.Add( Pointer(StrToInt(t[1])+42) );
                        List.Add( Pointer(pos(t[Length(t)],bc_CodeChar[bc_93])-1));
                      end;

                      Key:=0; Key2:=0;

                      for i:=List.Count-1 DownTo 0 do
                      begin Key :=Key +(List.Count  -i)*Integer(List[i]);
                            Key2:=Key2+(List.Count+1-i)*Integer(List[i]);
                      end;

                      List.Add( Pointer(   Key     mod 47) );
                      List.Add( Pointer((Key2+Key) mod 47) );

                      t:=  '101011110';
                      for i:=0 to List.Count-1 do t:=t+C93[Integer(List[i])];
                      t:=t+'1010111101';
                      List.Free;
                end;
     bc_BAR:    begin t:='10001000101110';
                      for i:=1 to Length(_code) do
                      begin
                        P:=Pos(_code[i],bc_CodeChar[CodeType])-1;
                        t:=t+CBar[P];
                      end;
                      t:=t+'10111000100010';
                end;
      bc_128A,bc_128B
               :begin if CodeType=bc_128A then key:=103
                                          else key:=104;
                      t:=C128[Key];

                      for i:=1 to Length(_code) do
                      begin P:=Pos(_code[i],bc_CodeChar[CodeType])-1;
                            t:=t+C128[P];
                            key:=key+i*P;
                      end;

                      key:=key mod 103;
                      t:=t+C128[key]+'1100011101011';
                end;
      bc_128C  :begin key:=105;
                      if Length(_Code) mod 2=0 then s:=    _Code
                                               else s:='0'+_Code;
                      t:=C128[Key];

                      i:=0;
                      While 2*i<Length(S) do
                      begin
                        P:=StrToInt(Copy(s,1+i*2,2));
                        Inc(i,1);
                        t:=t+C128[P];
                        key:=key+i*P;
                      end;

                      key:=key mod 103;
                      t:=t+C128[key]+'1100011101011';
                end;
    end;
    { AddOn2 & AddOn5  {

    if CodeType in [bc_EAN8, bc_EAN13]
    then begin
           Key:=StrToInt(_code[1])*10+StrToInt(_code[2]);
           Key:=Key mod 4;
           s:='00000000003033'+EAN[StrToInt(_code[1]),ParitetAdd[Key,1]]
                         +'03'+EAN[StrToInt(_code[2]),ParitetAdd[Key,2]];
           While 0<Pos('2',s) do s[Pos('2',s)]:='3';
           t:=t+s;
         end;

    if CodeType in [bc_UPCA, bc_UPCE]
    then begin
           Key:=StrToInt(_code[1])  +StrToInt(_code[2])  +StrToInt(_code[3])
               +StrToInt(_code[4])*9+StrToInt(_code[5])*3;
           Key:=Key mod 10;
           s:='00000000003033'+EAN[StrToInt(_code[1]),ParitetUPC[Key,2]]
                         +'03'+EAN[StrToInt(_code[2]),ParitetUPC[Key,3]]
                         +'03'+EAN[StrToInt(_code[3]),ParitetUPC[Key,4]]
                         +'03'+EAN[StrToInt(_code[4]),ParitetUPC[Key,5]]
                         +'03'+EAN[StrToInt(_code[5]),ParitetUPC[Key,6]];
           While 0<Pos('2',s) do s[Pos('2',s)]:='3';
           t:=t+s;
         end;
     {}
    _line:='000'+t+'000';
    Result:=True;
  except
    _line:='0';
    Result:=False;
  end;
end;

//------------------------------------------------------------------------------

{ TBarCode }

constructor TBarCode.Create;
begin
  FCodeType:= bc_None;
  FCode:= EmptyStr;
  FCheckCode:= EmptyStr;
  FLineCode:= EmptyStr;
  FQRCode:= TDelphiZXingQRCode.Create;
end;

destructor TBarCode.Destroy;
begin
  FQRCode.Free;
  inherited;
end;

function TBarCode.CheckCode: Boolean;
begin
//  Result:= Length(FLineCode);
end;

function TBarCode.Width: Integer;
begin
  Result:= Length(FLineCode);
end;

procedure TBarCode.PaintBarCode(TC: TCanvas; QuietZone: Integer);
var _Width, i,j,H,HH,Z,Quiet,QuietX,QuietY : Integer;
    _Color         : TColor;
begin
  if TC = nil then
    raise Exception.Create('PaintBarCode Canvas is nil');
  if 0=Length(FLineCode) then
    Exit;

  _Width:= TC.Pen.Width;
  _Color:= TC.Brush.Color;

  TC.Pen.Width:= 1;
  TC.Brush.Color:= TC.Pen.Color;

  if (QuietZone < 0) or (QuietZone*2 >= TC.ClipRect.Height) or (QuietZone*2 >= TC.ClipRect.Width)
    then Quiet:= Min(TC.ClipRect.Height, TC.ClipRect.Width) div 10
    else Quiet:= QuietZone;

  if FCodeType= bc_QR
    then Z:= (Min(TC.ClipRect.Width, TC.ClipRect.Height)- 2*Quiet) div (FQRCode.Rows-2*FQRCode.QuietZone)
    else Z:= (TC.ClipRect.Width - 2*Quiet) div Length(FLineCode);

  QuietX:= (TC.ClipRect.Width - Z*Length(FLineCode)) div 2;
  QuietY:= Quiet;

  if FCodeType= bc_QR then
    begin
      for i:= 0 to Pred(FQRCode.Rows-2*FQRCode.QuietZone) do
        for j:= 0 to Pred(FQRCode.Columns-2*FQRCode.QuietZone) do
          if FQRCode.Elements[i][j] then
            TC.Rectangle(QuietX+i*Z, QuietY+j*Z, QuietX+Succ(i)*Z, QuietY+Succ(j)*z );

    end
  else
    begin
      HH:= TC.ClipRect.Height - 2*QuietY;
      h:= HH div 8;
      for i:=1 to Length(FLineCode) do
        case FLineCode[i] of
         '0': ;
         '1': TC.Rectangle(QuietX+i*Z, QuietY,   QuietX+Succ(i)*Z, QuietY+HH  );
         '2': TC.Rectangle(QuietX+i*Z, QuietY,   QuietX+Succ(i)*Z, QuietY+HH-H);
         '3': TC.Rectangle(QuietX+i*Z, QuietY+H, QuietX+Succ(i)*Z, QuietY+HH-H);
        end;
    end;

  TC.Pen.Width:= _Width;
  TC.Brush.Color:= _Color;

end;

procedure TBarCode.SaveToFile(aFileName: String; PixelSize: Integer);
  var BitMap: TBitMap;
begin
  BitMap:= TBitMap.Create;
  BitMap.Width:= Length(FLineCode) * 32;
  if FCodeType = bc_QR then BitMap.Height:= BitMap.Width
                       else BitMap.Height:= BitMap.Width div 4;

  BitMap.Canvas.Pen.Color:= clBlack;
  PaintBarCode(BitMap.Canvas, 1);

  BitMap.SaveToFile(aFileName);
  Bitmap.Free;
end;

function TBarCode.SetCode(aCodeType: Integer; aCode: String): Boolean;
var i: Integer;
    s: String;
begin
  FCode:= aCode;
  FCodeType:= aCodeType;

  if aCodeType = bc_auto then
    begin
      for i:=1 to bc_TypeCodeCount-1 do
        if bc_IsCodeType(i, aCode, s) then
          begin
            FCodeType:= i;
            Break;
          end;

      if FCodeType = bc_auto then
        if Length(aCode)=0 then FCodeType:= bc_None
                           else FCodeType:= bc_QR;
    end;

  if FCodeType= bc_QR then
    with FQRCode do
      begin
      //  BeginUpdate;
        Data:= FCode;
        RegisterEncoder(ENCODING_WIN1251, TWin1251Encoder);
        Encoding := ENCODING_WIN1251; //  ENCODING_AUTO = 0; ENCODING_NUMERIC = 1; ENCODING_ALPHANUMERIC = 2;
                                      //  ENCODING_8BIT = 3; ENCODING_UTF8_NOBOM = 4; ENCODING_UTF8_BOM = 5;
        ErrorCorrectionOrdinal:= ecoM;   //(ecoL, ecoM, ecoQ, ecoH);
        QuietZone:= 2;
   //     EndUpdate;
        FLineCode:= EmptyStr;
        while Length(FLineCode) < FQRCode.Rows-2*QuietZone do
          FLineCode:= FLineCode + '9';
        Result:= True;
      end
  else
    begin
      Result:= bc_GetLineCode(FCodeType, FCode, FLineCode);
    end;
end;

end.
