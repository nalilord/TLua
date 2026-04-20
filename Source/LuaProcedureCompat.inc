{$IFDEF FPC}
  TLuaProcedure = procedure(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
{$ELSE}
  TLuaProcedure = reference to procedure(Sender: TLua; Args: TLuaArgs; Results: TLuaResults);
{$ENDIF}
