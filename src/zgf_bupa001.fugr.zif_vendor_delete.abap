FUNCTION ZIF_VENDOR_DELETE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_LIFNR) TYPE  LIFNR OPTIONAL
*"     VALUE(I_CPF_CNPJ) TYPE  LFA1-STCD1 OPTIONAL
*"  TABLES
*"      E_MESS_TAB STRUCTURE  ZPF_MENSAGEM OPTIONAL
*"----------------------------------------------------------------------
  CLEAR e_mess_tab.

  e_mess_tab-type   = 'E'.
  e_mess_tab-texto  = 'Sem autorização para excluir fornecedor'(002).

  APPEND e_mess_tab TO e_mess_tab.

  RETURN.


ENDFUNCTION.
