unit uPathXML;

interface

uses Xml.XMLIntf;

function FindNodeByPathXML(const Node: IXMLNode; const Path: string; const AutoCreated: Boolean = False): IXMLNode;

function ValueFromPathXML(const Node: IXMLNode; const Path: string): OleVariant;
function BodyFromPathXML(const Node: IXMLNode; const Path: string): String;
function XMLFromPathXML(const Node: IXMLNode; const Path: string): Variant;
function ValueToPathXML(const Node: IXMLNode; const Path: string; const Value: OleVariant): Boolean;

implementation

uses SysUtils, Variants, Classes, Xml.XMLDOM
     {$IF DEFINED(DEBUG) or DEFINED(DeDEBUG)}, DeLog{$ENDIF};

function FindNodeByPathXML(const Node: IXMLNode; const Path: string; const AutoCreated: Boolean): IXMLNode;
var
  Value, Name, SubPath: string;
  Index, NodeIndex: Integer;
begin
  if Assigned(Node) then
    begin
      SubPath := Path;
      if Length(SubPath) = 0 then
        Result := Node
      else
        begin
          if SubPath[1] = '\' then Delete(SubPath, 1, 1);
          Index := Pos('\', SubPath);
          if Index = 0 then
            begin
              Name := SubPath;
              SubPath := EmptyStr;
            end
          else
            begin
              Name := Copy(SubPath, 1, Pred(Index));
              SubPath := Copy(SubPath, Index, Length(SubPath));
            end;
          if Length(Name) = 0 then
            Result := Node
          else
            if Name[1] = '@' then
              begin
                Delete(Name, 1, 1);
                if Length(Name) = 0 then
                  begin
                    Result := nil;
                    {$IFDEF DeDEBUG}
                    WriteLog('Syntax error in path XML ' + QuotedStr(Path) + ': empty attribute name not supported');
                    {$ENDIF}
                    Exit;
                  end
                else
                  if Length(SubPath) = 0 then
                    begin
                      if Assigned(Node.AttributeNodes) then
                        begin
                          Result := Node.AttributeNodes.FindNode(Name);
                          if AutoCreated and not Assigned(Result) then
                            begin
                              Node.Attributes[Name] := EmptyStr;
                              Result := Node.AttributeNodes.FindNode(Name);
                            end;
                        end
                      else
                        Result := nil;
                    end
                  else
                    begin
                      Result := nil;
                      {$IFDEF DeDEBUG}
                      WriteLog('Syntax error in path XML ' + QuotedStr(Path) + ': sub path in attribute ' + QuotedStr(Name) + ' not supported');
                      {$ENDIF}
                      Exit;
                    end;
              end
            else
              begin
                Index := Pos('[', Name);
                if Index = 0 then
                  Value := EmptyStr
                else
                  begin
                    Value := Copy(Name, Succ(Index), Length(Name));
                    Name := Copy(Name, 1, Pred(Index));
                    Index := Pos(']', Value);
                    if Index = 0 then
                      begin
                        Result := nil;
                        {$IFDEF DeDEBUG}
                        WriteLog('Syntax error in path XML ' + QuotedStr(Path) + ': range symbol '']'' not found');
                        {$ENDIF}
                        Exit;
                      end
                    else
                      if Length(Copy(Value, Succ(Index), Length(Value))) = 0 then
                        begin
                          Value := Copy(Value, 1, Pred(Index));
                          if Length(Trim(Value)) = 0 then
                            begin
                              Result := nil;
                              {$IFDEF DeDEBUG}
                              WriteLog('Syntax error in path XML ' + QuotedStr(Path) + ': empty range not supported');
                              {$ENDIF}
                              Exit;
                            end;
                        end
                      else
                        begin
                          Result := nil;
                          {$IFDEF DeDEBUG}
                          WriteLog('Syntax error in path XML ' + QuotedStr(Path) + ': unknown after range string');
                          {$ENDIF}
                          Exit;
                        end;
                  end;
                // Если ищем узел без индекса, то ...
                if Length(Value) = 0 then
                  begin
                    if SameText(Name, '.') or SameText(Node.NodeName, Name) then
                      if Length(SubPath) = 0 then
                        Result := Node
                      else
                        Result := FindNodeByPathXML(Node, SubPath, AutoCreated)
                    else
                      if SameText(Name, '..') then
                        if Length(SubPath) = 0 then
                          Result := Node.ParentNode
                        else
                          if Assigned(Node.ParentNode) then
                            Result := FindNodeByPathXML(Node.ParentNode, SubPath, AutoCreated)
                          else
                            begin
                              Result := nil;
                              {$IFDEF DeDEBUG}
                              WriteLog('Syntax error in path XML ' + QuotedStr(Path) + ': parent node is nil');
                              {$ENDIF}
                            end
                      else
                        begin
                          Result := nil;
                          if Assigned(Node.ChildNodes) then
                            begin
                              for Index := 0 to Pred(Node.ChildNodes.Count) do
                                if SameText(Node.ChildNodes[Index].NodeName, Name) then
                                  begin
                                    if Length(SubPath) = 0 then
                                      Result := Node.ChildNodes[Index]
                                    else
                                      Result := FindNodeByPathXML(Node.ChildNodes[Index], SubPath, AutoCreated);
                                    Break;
                                  end;
                              if AutoCreated and not Assigned(Result) then
                                begin
                                  Result := Node.AddChild(Name);
                                  if Length(SubPath) <> 0 then
                                    Result := FindNodeByPathXML(Result, SubPath, AutoCreated);
                                end;
                            end;
                        end;
                  end
                else
                  if TryStrToInt(Value, NodeIndex) then
                    if NodeIndex >= 0 then
                      begin
                        Result := nil;
                        if Assigned(Node.ChildNodes) then
                          begin
                            for Index := 0 to Pred(Node.ChildNodes.Count) do
                              if SameText(Node.ChildNodes[Index].NodeName, Name) then
                                if NodeIndex = 0 then
                                  begin
                                    if Length(SubPath) = 0 then
                                      Result := Node.ChildNodes[Index]
                                    else
                                      Result := FindNodeByPathXML(Node.ChildNodes[Index], SubPath, AutoCreated);
                                    Break;
                                  end
                                else
                                  Dec(NodeIndex);
                            if AutoCreated and not Assigned(Result) then
                              begin
                                repeat
                                  Result := Node.AddChild(Name);
                                  Dec(NodeIndex);
                                until NodeIndex < 0;
                                if Length(SubPath) <> 0 then
                                  Result := FindNodeByPathXML(Result, SubPath, AutoCreated);
                              end;
                          end;
                      end
                    else
                      begin
                        Result := nil;
                        {$IFDEF DeDEBUG}
                        WriteLog('Syntax error in path XML ' + QuotedStr(Path) + ': range index ' + IntToStr(NodeIndex) + ' not between 0 and ' + IntToStr(MaxInt));
                        {$ENDIF}
                      end
                  else
                    begin
                      Result := nil;
                      {$IFDEF DeDEBUG}
                      WriteLog('Syntax error in path XML ' + QuotedStr(Path) + ': range ' + QuotedStr(Value) + ' not integer value');
                      {$ENDIF}
                    end;
              end

        end;
    end
  else
    Result := nil;
end;

function ValueFromPathXML(const Node: IXMLNode; const Path: string): OleVariant;
var
  WorkNode: IXMLNode;
begin
  WorkNode := FindNodeByPathXML(Node, Path, False);
  if Assigned(WorkNode) then
    Result := WorkNode.NodeValue
  else
    Result := Null;
end;

function BodyFromPathXML(const Node: IXMLNode; const Path: string): String;
var
  WorkNode: IXMLNode;
begin
  WorkNode := FindNodeByPathXML(Node, Path, False);
  if Assigned(WorkNode) then
    Result := WorkNode.XML
  else
    Result := Null;
end;

function XMLFromPathXML(const Node: IXMLNode; const Path: string): Variant;
var
  Name, SubPath: string;
  Index, Counter: Integer;
  procedure AppendItem(const Value: Variant);
  var
    Size: Integer;
    TempValue: Variant;
  begin
    case Counter of
      0: { Пустрой список }
        Result := Value;
      1: { Одно значение }
        begin
          TempValue := Result;
          Result := VarArrayCreate([0, 1], varVariant);
          VarArrayPut(Result, TempValue, [0]);
          VarArrayPut(Result, Value, [1]);
        end
    else
      Size := Succ(VarArrayHighBound(Result, 1));
      VarArrayRedim(Result, Size);
      VarArrayPut(Result, Value, [Size]);
    end;
    Inc(Counter);
  end;
  procedure AppendNode(const Node: IXMLNode);
  begin
    if Assigned(Node) then
      AppendItem(Node.XML)
    else
      AppendItem(Null);
  end;
  procedure AppendList(const Value: Variant);
  var
    Index: Integer;
  begin
    if not VarIsEmpty(Value) then
      if VarIsArray(Value) then
        for Index := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
          AppendItem(VarArrayGet(Value, [Index]))
      else
        AppendItem(Value);
  end;
begin
  Result := Unassigned;
  Counter := 0;
  if Assigned(Node) then
    begin
      SubPath := Path;
      if Length(SubPath) = 0 then
        AppendItem(Node)
      else
        begin
          if SubPath[1] = '\' then Delete(SubPath, 1, 1);
          Index := Pos('\', SubPath);
          if Index = 0 then
            begin
              Name := SubPath;
              SubPath := EmptyStr;
            end
          else
            begin
              Name := Copy(SubPath, 1, Pred(Index));
              SubPath := Copy(SubPath, Index, Length(SubPath));
            end;
          if Length(Name) = 0 then
            AppendNode(Node)
          else
            if SameText(Name, '.') or SameText(Node.NodeName, Name) then
              if Length(SubPath) = 0 then
                AppendNode(Node)
              else
                AppendList(XMLFromPathXML(Node, SubPath))
            else
              if SameText(Name, '..') then
                if Assigned(Node.ParentNode) then
                  if Length(SubPath) = 0 then
                    AppendNode(Node.ParentNode)
                  else
                    AppendList(XMLFromPathXML(Node.ParentNode, SubPath))
                else
                  begin
                    {$IFDEF DeDEBUG}
                    WriteLog('Syntax error in path XML ' + QuotedStr(Path) + ': parent node is nil');
                    {$ENDIF}
                  end
              else
                if Assigned(Node.ChildNodes) then
                  begin
                    for Index := 0 to Pred(Node.ChildNodes.Count) do
                      if SameText(Node.ChildNodes[Index].NodeName, Name) then
                        if Length(SubPath) = 0 then
                          AppendNode(Node.ChildNodes[Index])
                        else
                          AppendList(XMLFromPathXML(Node.ChildNodes[Index], SubPath));
                  end
                else
                  begin
                    {$IFDEF DeDEBUG}
                    WriteLog('Syntax error in path XML ' + QuotedStr(Path) + ': not assigned child nodes');
                    {$ENDIF}
                  end;
        end;
    end;
end;

function ValueToPathXML(const Node: IXMLNode; const Path: string; const Value: OleVariant): Boolean;
var
  WorkNode: IXMLNode;
begin
  WorkNode := FindNodeByPathXML(Node, Path, True);
  Result := Assigned(WorkNode);
  if Result then
    WorkNode.NodeValue := Value;
end;

{$IFDEF DEBUG}
initialization
  DebugLog('uPathXML unit initialization ...');

finalization
  DebugLog('uPathXML unit finalization ...');
{$ENDIF}

end.

