unit G2D;
{*****************************************************************************
 *
 *   Преобразования координат в двумерном пространстве
 *
 *
 ****************************************************************************}

interface

{ Точка представлена в однородных координатах}
type
  TPoint2D = packed record
    X,Y,W : single;
  end;
  PPoint2D = ^TPoint2D;

function  Point2D(X,Y:single):TPoint2D;
function  PointsEqual2D(P1,P2:TPoint2D):boolean;
function  OffsetPoint2D(aPoint:TPoint2D;dX,dY:single):TPoint2D;

// построение вектора по двум точкам
function  Vector2D(P1,P2:TPoint2D):TPoint2D;
// длинна вектора V
function  VLength2D(V:TPoint2D):single;
// нормировка вектора V
function  Norm2D(V:TPoint2D; aLength: Single = 1):TPoint2D;
// нормировка вектора V возвращает 0 если вектор не определен
function  Norm2Dalt(V:TPoint2D; aLength: Single = 1):TPoint2D;
// расстоание между точками
function  Distance2D(P1,P2:tPoint2D):single;
// скалярное произведение векторов
function  ScalarVV2D(V1,V2:tPoint2D):single;
// величина векторного произведения векторов (со знаком!)
function  VectorVV2D(V1,V2:tPoint2D):single;

// расстояние от точки P до отрезка (P1,P2)
function  DistancePV2D(P,P1,P2:TPoint2D):single;
// расстояние от точки P до прямой со знаком
function  DistancePL(P,P1,P2:TPoint2D):single;

type
  TRect2D  = packed record
    case boolean of
    TRUE:(TopLeft,BottomRight    : TPoint2D);
    FALSE:(Left,Top,W1,Right,Bottom,W2 : single);
  end;

// построение прямоугольника, параллельного осям координат
function Rect2D(X1,Y1,X2,Y2:single):TRect2D;
// построение прямоугольника с возрастающими точками границ
function BoundRect2D(R1: TRect2D): TRect2D;
// проверка на равенство прямоугольников
function RectsEqual2D(const R1,R2: TRect2D):boolean;
// определение "пустоты" прямоугольника
function IsEmptyRect2D(const R: TRect2D): Boolean;
// расширение прямоугольника для включения заданой точки
function ExpandRect2D(const R:TRect2D;const P:TPoint2D):TRect2D;
// определение области пересечения прямоугольников
function IntersectRect2D(out Rect: TRect2D; const R1, R2: TRect2D): Boolean;
// определение минимального прямоугольника, содержащего оба данных
function UnionRect2D(out Rect: TRect2D; const R1, R2: TRect2D): Boolean;
// "растягивание" прямоугольника на заданную величину
procedure InflateRect2D(var Rect: TRect2D; dX,dY:single);
// определение попадания точки в прямоугольRightник
function PointInRect2D(const Point:TPoint2D;const Rect:TRect2D):boolean;

function RectTopLeft(const Rect:TRect2D):TPoint2D;
function RectTop(const Rect:TRect2D):TPoint2D;
function RectTopRight(const Rect:TRect2D):TPoint2D;
function RectRight(const Rect:TRect2D):TPoint2D;
function RectBottomRight(const Rect:TRect2D):TPoint2D;
function RectBottom(const Rect:TRect2D):TPoint2D;
function RectBottomLeft(const Rect:TRect2D):TPoint2D;
function RectLeft(const Rect:TRect2D):TPoint2D;
function RectCenter(const Rect:TRect2D):TPoint2D;
function RectSizes(const Rect:TRect2D):TPoint2D;


{ Матрица преобразования однородных координат}
type
  PMatrix2D = ^TMatrix2D;
  TMatrix2D = packed record
    M11,M12,M21,M22,M31,M32:single;
  end;

function  Matrix2D(A11,A12,A21,A22,A31,A32:single):tMatrix2D;

{ процедура умножения вектора на матрицу}
function  MultVM2D(V:TPoint2D;const M:TMatrix2D):TPoint2D;

{ процедура умножения матрицы на матрицу}
function  MultMM2D(M1,M2:TMatrix2D):TMatrix2D;

{ построение единичной матрицы}
procedure M2D_Identity(var M:TMatrix2D);

{ добавление переноса}
function  M2D_Shift(M:TMatrix2D;ShiftX,ShiftY:single):TMatrix2D;

{ добавление поворота относительно центра координат}
function  M2D_Rotate(M:TMatrix2D;Alpha:single):TMatrix2D;

{  добавление масштабирования относительно центра координат}
function  M2D_Scale(M:TMatrix2D;Kx,Ky:single):TMatrix2D;
function  M2D_ScaleX(M:TMatrix2D;K:single):TMatrix2D;
function  M2D_ScaleY(M:TMatrix2D;K:single):TMatrix2D;

{ проверка матрицы на единичность}
function  M2D_IsIdentity(M:TMatrix2D):boolean;
{ проверка матриц на идентичность}
function  M2D_IsEqual(M1,M2:TMatrix2D):boolean;

{ построение обратной матрицы}
function  M2D_Reverse(M:TMatrix2D):TMatrix2D;


function  Bezier2D(P0,P1,P2,P3:TPoint2D;t:double):TPoint2D;


implementation

uses
  SysUtils,
  Math;

const
  Eps = 1e-20;

function  Point2D(X,Y:single):TPoint2D;
begin
  Result.X:=X;
  Result.Y:=Y;
  Result.W:=1;
end;

function  PointsEqual2D(P1,P2:TPoint2D):boolean;
begin
  Result := (abs(P1.X-P2.X)<=Eps)and(abs(P1.Y-P2.Y)<=Eps);
end;

function  OffsetPoint2D(aPoint:TPoint2D;dX,dY:single):TPoint2D;
begin
  Result.X:=aPoint.X+dX;
  Result.Y:=aPoint.Y+dY;
  Result.W:=1;
end;

function  Vector2D(P1,P2:TPoint2D):TPoint2D;
begin
  Result := Point2D(P2.X-P1.X,P2.Y-P1.Y);
end;

function  VLength2D(V:TPoint2D):single;
begin
  Result := sqrt(sqr(V.X)+sqr(V.Y));
end;

function  Norm2D(V:TPoint2D; aLength: Single = 1):TPoint2D;
var
  d : single;
begin
  d := sqrt(sqr(V.X)+sqr(V.Y));
  if d<1e-100 then
    raise Exception.Create('Vector is NULL!')
  else
    Result := Point2D(aLength*V.X/d,aLength*V.Y/d);
end;

function  Norm2Dalt(V:TPoint2D; aLength: Single = 1):TPoint2D;
var d : single;
begin
  d := sqrt(sqr(V.X)+sqr(V.Y));
  if d<1e-100 then
    Result := Point2D(0,0)
  else
    Result := Point2D(aLength*V.X/d,aLength*V.Y/d);
end;

function  Distance2D(P1,P2:tPoint2D):single;
var
  V : TPoint2D;
begin
  V := Vector2D(P1,P2);
  Result := sqrt(sqr(V.X)+sqr(V.Y));
end;

function  ScalarVV2D(V1,V2:tPoint2D):single;
begin
  Result := V1.X*V2.X+V1.Y*V2.Y;
end;

function  VectorVV2D(V1,V2:tPoint2D):single;
begin
  Result := V1.X*V2.Y-V1.Y*V2.X;
end;

function  DistancePV2D(P,P1,P2:TPoint2D):single;
var
  V1,V2  : TPoint2D;
  d,r,t  : single;
begin
  d  := Distance2D(P1,P2);
  if d=0 then
    Result := Distance2D(P1,P)
  else begin
    V1 := Vector2D(P1,P2);
    V2 := Vector2D(P1,P);
    r  := abs(VectorVV2D(V1,V2))/d;//расстояние от точки до прямой
                                   //содержащей отрезок
    t  := ScalarVV2D(V1,V2)/d;     //проекция вектора (P1,P)на вектор (P1,P2)
    if (t<0) then
      Result := sqrt(sqr(t)+sqr(r))
    else if (t>d) then
      Result := sqrt(sqr(t-d)+sqr(r))
    else
      Result := r;
  end;
end;

function  DistancePL(P,P1,P2:TPoint2D):single;
var
  V1,V2  : TPoint2D;
  d      : single;
begin
  d  := Distance2D(P1,P2);
  if d=0 then
    Result := Distance2D(P1,P)
  else begin
    V1 := Vector2D(P1,P2);
    V2 := Vector2D(P1,P);
    Result := (VectorVV2D(V1,V2))/d;
  end;
end;

function  Rect2D(X1,Y1,X2,Y2:single):TRect2D;
begin
  Result.TopLeft     := Point2D(X1,Y1);
  Result.BottomRight := Point2D(X2,Y2);
end;

function BoundRect2D(R1: TRect2D): TRect2D;
begin
  Result.Left  := Min(R1.Left,R1.Right);
  Result.Right := Max(R1.Left,R1.Right);
  Result.Bottom:= Min(R1.Bottom,R1.Top);
  Result.Top   := Max(R1.Bottom,R1.Top);
end;

function RectsEqual2D(const R1,R2: TRect2D):boolean;
begin
  Result := PointsEqual2D(R1.TopLeft,R2.TopLeft)
            and PointsEqual2D(R1.BottomRight,R2.BottomRight);
end;

function  IsEmptyRect2D(const R: TRect2D): Boolean;
begin
  {Result := (R.Left>=R.Right)or(R.Bottom>=R.Top);}
  Result := ((R.Left<R.Right)and(R.Bottom<R.Top))
            or((R.Left=R.Right)and(R.Bottom<R.Top))
            or((R.Left<R.Right)and(R.Bottom=R.Top));
  Result := not Result;          
end;

function  ExpandRect2D(const R:TRect2D;const P:TPoint2D):TRect2D;
begin
  Result := R;
  if Result.Left>P.X then
    Result.Left := P.X
  else if Result.Right<P.X then
    Result.Right := P.X;
  if Result.Top<P.Y then
    Result.Top := P.Y
  else if Result.Bottom>P.Y then
    Result.Bottom := P.Y;
end;

function  IntersectRect2D(out Rect: TRect2D; const R1, R2: TRect2D): Boolean;
begin
  Rect   := Rect2D(Max(R1.Left,R2.Left),Min(R1.Top,R2.Top),
                   Min(R1.Right,R2.Right),Max(R1.Bottom,R2.Bottom));
  Result := not IsEmptyRect2D(Rect);
end;

function  UnionRect2D(out Rect: TRect2D; const R1, R2: TRect2D): Boolean;
begin
  Rect   := R1;
  Rect   := ExpandRect2D(Rect,R2.TopLeft);
  Rect   := ExpandRect2D(Rect,R2.BottomRight);
  Result := not IsEmptyRect2D(Rect);
end;

procedure InflateRect2D(var Rect: TRect2D; dX,dY:single);
begin
  Rect := Rect2D(Rect.Left-dX,Rect.Top+dY,Rect.Right+dX,Rect.Bottom-dY);
end;

function  PointInRect2D(const Point:TPoint2D;const Rect:TRect2D):boolean;
begin
  Result := (Point.X>=Rect.Left)
            and(Point.X<=Rect.Right)
            and(Point.Y>=Rect.Bottom)
            and(Point.Y<=Rect.Top);
end;


function RectTopLeft(const Rect:TRect2D):TPoint2D;
begin
  Result := Rect.TopLeft;
end;

function RectTop(const Rect:TRect2D):TPoint2D;
begin
  Result := Point2D((Rect.Left+Rect.Right)/2,Rect.Top);
end;

function RectTopRight(const Rect:TRect2D):TPoint2D;
begin
  Result := Point2D(Rect.Right,Rect.Top);
end;

function RectRight(const Rect:TRect2D):TPoint2D;
begin
  Result := Point2D(Rect.Right,(Rect.Top+Rect.Bottom)/2);
end;

function RectBottomRight(const Rect:TRect2D):TPoint2D;
begin
  Result := Rect.BottomRight;
end;

function RectBottom(const Rect:TRect2D):TPoint2D;
begin
  Result := Point2D((Rect.Left+Rect.Right)/2,Rect.Bottom);
end;

function RectBottomLeft(const Rect:TRect2D):TPoint2D;
begin
  Result := Point2D(Rect.Left,Rect.Bottom);
end;

function RectLeft(const Rect:TRect2D):TPoint2D;
begin
  Result := Point2D(Rect.Left,(Rect.Top+Rect.Bottom)/2);
end;

function RectCenter(const Rect:TRect2D):TPoint2D;
begin
  Result := Point2D((Rect.Left+Rect.Right)/2,(Rect.Top+Rect.Bottom)/2);
end;

function RectSizes(const Rect:TRect2D):TPoint2D;
begin
  Result := Point2D(Rect.Right-Rect.Left,Rect.Top-Rect.Bottom);
end;

function  Matrix2D(A11,A12,A21,A22,A31,A32:single):tMatrix2D;
begin
  Result.M11:=A11;
  Result.M12:=A12;
  Result.M21:=A21;
  Result.M22:=A22;
  Result.M31:=A31;
  Result.M32:=A32;
end;

{ процедура умножения вектора на матрицу}
function  MultVM2D(V:TPoint2D;const M:TMatrix2D):TPoint2D;
begin
  // оптимизировано для нашего 2D случая
  Result.X := V.X*M.M11+V.Y*M.M21+M.M31;
  Result.Y := V.X*M.M12+V.Y*M.M22+M.M32;
  Result.W := 1;
end;

{ процедура умножения матрицы на матрицу}
function  MultMM2D(M1,M2:TMatrix2D):TMatrix2D;
begin
  // оптимизировано для нашего 2D случая
  Result.M11:=M1.M11*M2.M11 + M1.M12*M2.M21;
  Result.M12:=M1.M11*M2.M12 + M1.M12*M2.M22;
  //
  Result.M21:=M1.M21*M2.M11 + M1.M22*M2.M21;
  Result.M22:=M1.M21*M2.M12 + M1.M22*M2.M22;
  //
  Result.M31:=M1.M31*M2.M11 + M1.M32*M2.M21 + M2.M31;
  Result.M32:=M1.M31*M2.M12 + M1.M32*M2.M22 + M2.M32;
end;

{ построение единичной матрицы}
procedure M2D_Identity(var M:TMatrix2D);
begin
  M := Matrix2D(1,0,
                0,1,
                0,0);
end;

{ построение матрицы переноса}
function  M2D_Shift(M:TMatrix2D;ShiftX,ShiftY:single):TMatrix2D;
begin
  Result := MultMM2D(M,Matrix2D(1,      0,
                                0,      1,
                                ShiftX, ShiftY));
end;

{ построение матрицы поворота относительно центра координат}
function  M2D_Rotate(M:TMatrix2D;Alpha:single):TMatrix2D;
begin
  Result := MultMM2D(M,Matrix2D( cos(Alpha),-sin(Alpha),
                                 sin(Alpha), cos(Alpha),
                                          0,          0));
end;

{ построение матрицы масштабирование относительно центра координат}
function  M2D_Scale(M:TMatrix2D;Kx,Ky:single):TMatrix2D;
begin
  Result := MultMM2D(M,Matrix2D( Kx, 0,
                                  0, Ky,
                                  0, 0));
end;

function  M2D_ScaleX(M:TMatrix2D;K:single):TMatrix2D;
begin
  Result := MultMM2D(M,Matrix2D( K, 0,
                                 0, 1,
                                 0, 0));
end;

function  M2D_ScaleY(M:TMatrix2D;K:single):TMatrix2D;
begin
  Result := MultMM2D(M,Matrix2D( 1, 0,
                                 0, K,
                                 0, 0));
end;

function  M2D_IsIdentity(M:TMatrix2D):boolean;
begin
  Result := (abs(M.M11-1)<Eps)
            and(abs(M.M12-0)<Eps)
            and(abs(M.M21-0)<Eps)
            and(abs(M.M22-1)<Eps)
            and(abs(M.M31-0)<Eps)
            and(abs(M.M32-0)<Eps);
end;

function  M2D_IsEqual(M1,M2:TMatrix2D):boolean;
begin
  Result := (abs(M1.M11-M2.M11)<Eps)
            and(abs(M1.M12-M2.M12)<Eps)
            and(abs(M1.M21-M2.M21)<Eps)
            and(abs(M1.M22-M2.M22)<Eps)
            and(abs(M1.M31-M2.M31)<Eps)
            and(abs(M1.M32-M2.M32)<Eps);
end;

function  M2D_Reverse(M:TMatrix2D):TMatrix2D;
var
  detM : single;
begin
  detM := M.M11*M.M22
         -M.M12*M.M21;
  try
    Result.M11 := M.M22/detM;
    if abs(Result.M11) < Eps then Result.M11 := 0;
    Result.M12 :=-M.M12/detM;
    if abs(Result.M12) < Eps then Result.M12 := 0;
    Result.M21 :=-M.M21/detM;
    if abs(Result.M21) < Eps then Result.M21 := 0;
    Result.M22 := M.M11/detM;
    if abs(Result.M22) < Eps then Result.M22 := 0;
    Result.M31 := (M.M21*M.M32-M.M22*M.M31)/detM;
    if abs(Result.M31) < Eps then Result.M31 := 0;
    Result.M32 :=-(M.M11*M.M32-M.M12*M.M31)/detM;
    if abs(Result.M32) < Eps then Result.M32 := 0;
  except
    raise Exception.Create('Couldn''t reverse matrix.');
  end;
end;

function    Bezier2D(P0,P1,P2,P3:TPoint2D;t:double):TPoint2D;
var
  k0,k1,k2,k3 : double;
  a1,a2,b2    : double;
begin
  a2 := (1-t);
  a1 := a2*a2;
  b2 := t*t;
  
  k0 := a1*a2;  //(1-t)*(1-t)*(1-t);
  k1 := 3*a1*t; //3*(1-t)*(1-t)*t;
  k2 := 3*a2*b2;//3*(1-t)*t*t;
  k3 := b2*t;   //t*t*t;
  Result := Point2D(k0*P0.X+k1*P1.X+k2*P2.X+k3*P3.X,
                    k0*P0.Y+k1*P1.Y+k2*P2.Y+k3*P3.Y);
end;


end.
