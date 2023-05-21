*&---------------------------------------------------------------------*
*& Include          Z_RELATORIO_CLASS
*&---------------------------------------------------------------------*


CLASS z_relatorio DEFINITION.
  PUBLIC SECTION.
    METHODS: get_data IMPORTING VALUE(it_ranges_ebeln) TYPE tt_range_ebeln
                                VALUE(it_ranges_ebelp) TYPE tt_range_ebelp
                                VALUE(it_ranges_aedat) TYPE tt_range_aedat
                                VALUE(it_ranges_ernam) TYPE tt_range_ernam,
      display_data_reuse_list,
      display_data_reuse_grid,
      display_data_salv.

    METHODS constructor.

  PRIVATE SECTION.

    DATA: it_fieldcat TYPE slis_t_fieldcat_alv.

    DATA: ls_layout TYPE slis_layout_alv.

    DATA: lt_ekko      TYPE TABLE OF ekko,
          wa_ekko      LIKE LINE OF lt_ekko,
          lt_ekpo      TYPE TABLE OF ekpo,
          wa_ekpo      LIKE LINE OF lt_ekpo,
          lt_ekko_ekpo TYPE TABLE OF zst_ekko_ekpo,
          wa_ekko_ekpo TYPE zst_ekko_ekpo.

    DATA: lo_alv_table     TYPE REF TO cl_salv_table,
          lo_alv_error     TYPE REF TO cx_salv_msg,
          lo_alv_functions TYPE REF TO cl_salv_functions,
          lv_text          TYPE string,
          lo_columns_tab   TYPE REF TO cl_salv_columns_table,
          lo_columns       TYPE REF TO cl_salv_column.

    METHODS: build_layout,
      build_layout_salv,
      build_fieldcat IMPORTING VALUE(p_fieldname) TYPE c
                               VALUE(p_seltext_l) TYPE c
*                               VALUE(p_just)      TYPE c
                               VALUE(p_outputlen) TYPE i,
      build_column IMPORTING VALUE(p_columnname) TYPE c
                             VALUE(p_longtext)   TYPE c,
      create_salv,
      pre_display_salv,
      pre_display.

ENDCLASS.

CLASS z_relatorio IMPLEMENTATION.
  METHOD constructor.
    me->pre_display( ).
    me->pre_display_salv( ).
  ENDMETHOD.

  METHOD get_data.

    SELECT ebeln
    aedat
    ernam
    FROM ekko
    INTO CORRESPONDING FIELDS OF TABLE lt_ekko
    WHERE ebeln IN it_ranges_ebeln
    AND aedat IN it_ranges_aedat
    AND ernam IN it_ranges_ernam .

    IF lt_ekko IS NOT INITIAL.

      SELECT ebeln
      ebelp
      bukrs
      werks
      FROM ekpo
      INTO CORRESPONDING FIELDS OF TABLE lt_ekpo
      FOR ALL ENTRIES IN lt_ekko
      WHERE ebeln = lt_ekko-ebeln
      AND ebelp IN it_ranges_ebelp.

      LOOP AT lt_ekpo INTO wa_ekpo.
        READ TABLE lt_ekko INTO wa_ekko WITH KEY ebeln = wa_ekpo-ebeln.
        IF sy-subrc = 0.
          wa_ekko_ekpo-ebeln = wa_ekko-ebeln.
          wa_ekko_ekpo-ebelp = wa_ekpo-ebelp.
          wa_ekko_ekpo-bukrs = wa_ekpo-bukrs.
          wa_ekko_ekpo-werks = wa_ekpo-werks.
          wa_ekko_ekpo-aedat = wa_ekko-aedat.
          wa_ekko_ekpo-ernam = wa_ekko-ernam.
          APPEND wa_ekko_ekpo TO lt_ekko_ekpo.
          CLEAR wa_ekko_ekpo.
        ENDIF.

      ENDLOOP.

      SORT lt_ekko_ekpo ASCENDING BY ebeln.

    ENDIF.
  ENDMETHOD.

  METHOD create_salv.

    TRY .
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_alv_table
          CHANGING
            t_table      = lt_ekko_ekpo.

      CATCH cx_salv_msg INTO lo_alv_error.
        lv_text = lo_alv_error->get_text( ).
        MESSAGE lv_text TYPE 'E'.

    ENDTRY.

  ENDMETHOD.


  METHOD build_layout.
    ls_layout-colwidth_optimize = abap_true.
    ls_layout-zebra = abap_true.
  ENDMETHOD.

  METHOD build_layout_salv.
    lo_alv_table->get_functions( )->set_all( abap_true ).
    lo_alv_table->get_columns( )->set_optimize( abap_true ).
    lo_alv_table->get_display_settings( )->set_striped_pattern( abap_true ).
    lo_alv_table->get_display_settings( )->set_list_header('Relatório de Compras').

  ENDMETHOD.

  METHOD build_fieldcat.
    DATA: ls_fieldcat TYPE slis_fieldcat_alv.
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = p_fieldname.
    ls_fieldcat-seltext_l = p_seltext_l.
    ls_fieldcat-just      = 'C'.
    ls_fieldcat-outputlen = p_outputlen.
    APPEND ls_fieldcat TO it_fieldcat.
  ENDMETHOD.

  METHOD build_column.

    DATA: not_found TYPE REF TO cx_salv_not_found.

    TRY .
        lo_columns = lo_alv_table->get_columns( )->get_column( p_columnname ).
        lo_columns->set_long_text( p_longtext ).
        lo_columns->set_medium_text('').
        lo_columns->set_short_text('').
        lo_columns->set_alignment( if_salv_c_alignment=>centered ).

      CATCH cx_salv_not_found INTO not_found.
        WRITE: / 'Error Handling'.
    ENDTRY.

  ENDMETHOD.

  METHOD pre_display.

    me->get_data( it_ranges_ebeln = lt_ranges_ebeln
    it_ranges_ebelp = lt_ranges_ebelp
    it_ranges_aedat = lt_ranges_aedat
    it_ranges_ernam = lt_ranges_ernam ).

    me->build_layout( ).

    me->build_fieldcat( p_fieldname = 'EBELN'
    p_seltext_l = 'Nº do documento de compras'
    p_outputlen = 24 ).

    me->build_fieldcat( p_fieldname = 'EBELP'
    p_seltext_l = 'Nº item do documento de compra'
    p_outputlen = 28 ).

    me->build_fieldcat( p_fieldname = 'BUKRS'
    p_seltext_l = 'Empresa'
    p_outputlen = 09 ).

    me->build_fieldcat( p_fieldname = 'WERKS'
    p_seltext_l = 'Centro'
    p_outputlen = 08 ).

    me->build_fieldcat( p_fieldname = 'AEDAT'
    p_seltext_l = 'Data de criação de documento de compras'
    p_outputlen = 32 ).

    me->build_fieldcat( p_fieldname = 'ERNAM'
    p_seltext_l = 'Usuário da pessoa que criou um documento de compras'
    p_outputlen = 36 ).

  ENDMETHOD.

  METHOD pre_display_salv.

    me->create_salv( ).

    me->build_layout_salv( ).

    me->build_column( p_columnname = 'EBELN'
    p_longtext = 'Nº do documento de compras' ).

    me->build_column( p_columnname = 'EBELN'
    p_longtext = 'Nº do documento de compras' ).

    me->build_column( p_columnname = 'EBELP'
    p_longtext = 'Nº item do documento de compra' ).

    me->build_column( p_columnname = 'BUKRS'
    p_longtext = 'Empresa' ).

    me->build_column( p_columnname = 'WERKS'
    p_longtext = 'Centro' ).

    me->build_column( p_columnname = 'AEDAT'
    p_longtext = 'Data de criação' ).

    me->build_column( p_columnname = 'ERNAM'
    p_longtext = 'Usuário' ).
  ENDMETHOD.

  METHOD display_data_reuse_list.

    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program = sy-repid
        is_layout          = ls_layout
        it_fieldcat        = it_fieldcat
      TABLES
        t_outtab           = lt_ekko_ekpo.

    IF sy-subrc <> 0.
* Implement suitable error handling here
      WRITE: / 'Exception error.'.
    ENDIF.

  ENDMETHOD.

  METHOD display_data_reuse_grid.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = 'SY-REPID'
        i_grid_title       = 'Relatório de Compras'
        is_layout          = ls_layout
        it_fieldcat        = it_fieldcat
      TABLES
        t_outtab           = lt_ekko_ekpo.

  ENDMETHOD.

  METHOD display_data_salv.

    lo_alv_table->display( ).

  ENDMETHOD.
ENDCLASS.
