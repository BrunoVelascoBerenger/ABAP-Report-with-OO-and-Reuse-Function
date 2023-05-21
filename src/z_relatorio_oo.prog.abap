*&---------------------------------------------------------------------*
*& Report Z_RELATORIO_OO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_relatorio_oo.

INCLUDE: z_relatorio_topOO,
z_relatorio_selOO,
z_relatorio_class.

START-OF-SELECTION.

  DATA: go_relatorio TYPE REF TO z_relatorio.

  go_relatorio =  NEW z_relatorio( ).

  CASE abap_true.
    WHEN p_list.
      go_relatorio->display_data_reuse_list( ).
    WHEN p_grid.
      go_relatorio->display_data_reuse_grid( ).
    WHEN p_salv.
      go_relatorio->display_data_salv( ).
  ENDCASE.
