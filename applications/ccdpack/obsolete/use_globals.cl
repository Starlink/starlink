procedure use_globals( action )
bool action {"yes",prompt="Use CCDPACK global variables"}
begin
   set CCDPACK_GLOBALS=action
end
