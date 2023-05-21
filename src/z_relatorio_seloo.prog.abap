*&---------------------------------------------------------------------*
*& Include          Z_RELATORIO_SELOO
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: so_ebeln FOR ekko-ebeln OBLIGATORY,
  so_ebelp FOR ekpo-ebelp,
  so_aedat FOR ekko-aedat,
  so_ernam FOR ekko-ernam NO INTERVALS NO-EXTENSION.

SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.

  PARAMETERS: p_list RADIOBUTTON GROUP alv,
  p_grid RADIOBUTTON GROUP alv,
  p_salv RADIOBUTTON GROUP alv.

SELECTION-SCREEN END OF BLOCK b2.


START-OF-SELECTION.

*Estrutura EBELN

IF so_ebeln IS NOT INITIAL.

  APPEND so_ebeln TO lt_ranges_ebeln.

ENDIF.


*Estrutura EBELP

IF so_ebelp IS NOT INITIAL.

  APPEND so_ebelp TO lt_ranges_ebelp.

ENDIF.


*Estrutura AEDAT

IF so_aedat IS NOT INITIAL.

  APPEND so_aedat TO lt_ranges_aedat.

ENDIF.

*Estrutura ERNAM

IF so_ernam IS NOT INITIAL.

  APPEND so_ernam TO lt_ranges_ernam.

ENDIF.
