*&---------------------------------------------------------------------*
*&  Include           ZMMSC001_C02_DUN
*&---------------------------------------------------------------------*

CLASS lcl_controller_dun DEFINITION.

  PUBLIC SECTION.

    TYPES: tt_fields TYPE STANDARD TABLE OF zecv_t002 WITH EMPTY KEY.

    CONSTANTS mc_type TYPE zecv_t001-type VALUE 'P'.
    DATA: mo_db_api_control TYPE REF TO zcl_db_api_controller.
    DATA mo_api_screening TYPE REF TO zcl_db_screening_controller.
    DATA: mo_alv TYPE REF TO zcl_ca_alv.
    DATA: mo_container TYPE REF TO cl_gui_custom_container.
    DATA: mc_container_name TYPE char50 VALUE 'GC_DUN_CONTAINER'.
    DATA: mo_alv_data TYPE REF TO data.
    DATA: mo_alv_data_line TYPE REF TO data.
    DATA: mv_supplier TYPE lifnr.
    DATA: gt_turn TYPE zcl_myc_master_data=>tt_supp_turnover.
    DATA: mt_integration_data TYPE zcl_int_ecv_integration=>tt_integration_data.

    CONSTANTS: BEGIN OF cs_alv_functions,
                 update TYPE salv_de_function  VALUE 'UPDATE',
               END OF cs_alv_functions.


    METHODS: create_alv_data.

    METHODS: show_alv.

    METHODS: get_supplier_data.

    METHODS on_user_command FOR EVENT added_function OF zcl_ca_alv
      IMPORTING e_salv_function.
    METHODS link_click FOR EVENT link_click OF zcl_ca_alv IMPORTING row column.

    METHODS: constructor.

  PROTECTED SECTION.

    CONSTANTS: BEGIN OF mc_fixed_columns,
                 supplier       TYPE string VALUE 'SUPPLIER',
                 vendor_desc    TYPE string VALUE 'VENDOR_DESC',
                 category1      TYPE string VALUE 'CATEGORY1',
                 category1_desc TYPE string VALUE 'CATEGORY1_DESC',
                 category2      TYPE string VALUE 'CATEGORY2',
                 category2_desc TYPE string VALUE 'CATEGORY2_DESC',
                 category3      TYPE string VALUE 'CATEGORY3',
                 category3_desc TYPE string VALUE 'CATEGORY3_DESC',
               END OF mc_fixed_columns.

    TYPES: BEGIN OF ts_supplier_data.
             INCLUDE TYPE zint_i_general_vendor_md.
           TYPES:
                    lifnr TYPE lfa1-lifnr,
                    name1 TYPE lfa1-name1,
                    stcd1 TYPE lfa1-stcd1,
                    land1 TYPE lfa1-land1,
                  END OF ts_supplier_data.
    TYPES: tt_supplier_data TYPE STANDARD TABLE OF ts_supplier_data.
    TYPES: BEGIN OF ts_screening_data.
             INCLUDE TYPE zcl_db_screening_controller=>ts_vendors.
             INCLUDE TYPE zcl_db_screening_controller=>ts_base_inquiry_id.
           TYPES:
                    entity_id        TYPE string,
                    event_categories TYPE string,
                    file_contents    TYPE zcl_db_screening_controller=>tt_files,
                    duns             TYPE zdun_e_duns,
                  END OF ts_screening_data.
    TYPES: tt_screening_data TYPE STANDARD TABLE OF ts_screening_data WITH EMPTY KEY.

    DATA: mt_supplier_data TYPE tt_supplier_data.
    DATA: mt_risk_data TYPE TABLE OF zcl_db_api_controller=>ts_risk_final_data.
    DATA mt_screening_data TYPE tt_screening_data.

    METHODS: set_alv_columns.
    METHODS: update.
    METHODS: load_data_dun_bd.
    METHODS: get_duns_risk_data_from_api.
    METHODS: save_data.
    METHODS download_files_screening.


ENDCLASS .

CLASS lcl_controller_dun IMPLEMENTATION.

  METHOD constructor.

    mo_db_api_control =  NEW zcl_db_api_controller( iv_api_id = zcl_db_api_controller=>cv_api_dun_and_bradstreet iv_auto_connection = abap_false ).
    mo_api_screening =  NEW zcl_db_screening_controller( iv_api_id = zcl_db_screening_controller=>cv_api_screening
                                                        iv_auto_connection = abap_false ).

  ENDMETHOD.

  METHOD: get_supplier_data.

    FIELD-SYMBOLS <lv_lifnr> TYPE any.

    CHECK mo_db_api_control IS BOUND.

    CONSTANTS lc_lifnr TYPE char30 VALUE '(SAPMF02K)LFA1-LIFNR'.

    ASSIGN (lc_lifnr) TO <lv_lifnr>.

    CHECK <lv_lifnr> IS ASSIGNED.

    mv_supplier = <lv_lifnr>.

    load_data_dun_bd( ).

  ENDMETHOD.

  METHOD load_data_dun_bd.
    DATA lt_r_lifnr TYPE RANGE OF lifnr.
    DATA lt_r_duns TYPE RANGE OF zdun_e_duns .
    DATA: ls_risk_data TYPE zcl_db_api_controller=>ts_risk_final_data.
    DATA ls_screening_data TYPE ts_screening_data.

    SELECT a~*, b~lifnr, b~name1, b~stcd1, b~land1
      FROM zint_i_general_vendor_md( langu = @sy-langu ) AS a
      INNER JOIN lfa1 AS b
      ON b~lifnr EQ a~vendor
      WHERE a~vendor EQ @mv_supplier
        AND b~ktokk IN ( 'ZPRC', 'ZPRG', 'ZPRN', 'ZPRX', 'ZPMN' )
      INTO CORRESPONDING FIELDS OF TABLE @mt_supplier_data.
    IF sy-subrc = 0.

      "Montamos rango de proveedores
      DATA: lt_supp TYPE zcl_myc_master_data=>tt_lifnr.
      lt_supp = VALUE #( FOR <fs_sup> IN mt_supplier_data ( lifnr = CONV #( <fs_sup>-lifnr ) ) ).

      "Obtenemos la facturación de PUIG
      "Método de MYC
      DATA(lo_myc_master_data) = NEW zcl_myc_master_data( ).

      DATA(lv_from) = |01.{ sy-datum(4) - 1 }|.
      DATA(lv_to)   = |12.{ sy-datum(4) - 1 }|.

      lo_myc_master_data->get_supplier_turnover(
        EXPORTING
          it_suppliers = lt_supp                 " Número de cuenta del proveedor o acreedor
          iv_date_from = CONV #( lv_from )                 " Campo de tipo DATS
          iv_date_to   = CONV #( lv_to )                 " Campo de tipo DATS
              RECEIVING
                rt_turnover  = gt_turn                 " Número de cuenta del proveedor o acreedor
      ).


      lt_r_lifnr = VALUE #( FOR <wa> IN mt_supplier_data ( sign = 'I' option = 'EQ' low = <wa>-lifnr ) ).

      "Datos de cabecera por proveedor
      SELECT *
        FROM zdun_t001
        INTO TABLE @DATA(lt_dun_header)
        WHERE lifnr IN @lt_r_lifnr.

      IF sy-subrc EQ 0.

        lt_r_duns = VALUE #( FOR <wa2> IN lt_dun_header ( sign = 'I' option = 'EQ' low = <wa2>-duns ) ).

        "Campos guardados
        SELECT *
          FROM zdun_t002
          INTO TABLE @DATA(lt_dun_fields)
          WHERE duns IN @lt_r_duns.
        IF sy-subrc EQ 0.

          SELECT * INTO TABLE @DATA(lt_screening_files)
                 FROM zdun_t004
            WHERE lifnr IN @lt_r_lifnr.

          LOOP AT mt_supplier_data ASSIGNING FIELD-SYMBOL(<fs_supp>).

            READ TABLE lt_dun_header ASSIGNING FIELD-SYMBOL(<fs_header>)
            WITH KEY lifnr = <fs_supp>-lifnr.
            IF sy-subrc EQ 0
           AND <fs_header>-duns IS NOT INITIAL.
              CLEAR ls_risk_data.
              ls_screening_data = VALUE #( vendor = <fs_supp>-vendor
                                          vendor_desc = <fs_supp>-vendor_desc ).
              LOOP AT lt_dun_fields ASSIGNING FIELD-SYMBOL(<fs_fields>)
                WHERE duns = <fs_header>-duns.

                ASSIGN COMPONENT <fs_fields>-field_name OF STRUCTURE ls_risk_data TO FIELD-SYMBOL(<fs_field>).
                IF sy-subrc = 0.
                  <fs_field> = <fs_fields>-value.
                ENDIF.

                CASE <fs_fields>-field_name.
                  WHEN zcl_db_screening_controller=>cs_api_fields-categories.
                    ls_screening_data-event_categories = <fs_fields>-value.
                ENDCASE.

              ENDLOOP.


              "Obtenemos los campos calculados
              ls_risk_data-solvency = COND #( WHEN ls_risk_data-total_liabilities <> 0 THEN ( ls_risk_data-total_assets / ls_risk_data-total_liabilities )
                                              ELSE 0 ).
*            ls_risk_data-solvency = ls_risk_data-solvency * 100.

              ls_risk_data-profitability = COND #( WHEN ls_risk_data-total_net_revenues <> 0 THEN ( ls_risk_data-profit_after_tax / ls_risk_data-total_net_revenues )
                                              ELSE 0 ).
              ls_risk_data-profitability  = ls_risk_data-profitability  * 100.

              "Facturación de puig
              READ TABLE gt_turn ASSIGNING FIELD-SYMBOL(<fs_turn>)
              WITH KEY supplier = <fs_supp>-lifnr.
              IF sy-subrc EQ 0.
                ls_risk_data-puig_amount_purch = <fs_turn>-turnover.

                ls_risk_data-puig_dependency = COND #( WHEN ls_risk_data-total_net_revenues <> 0 THEN ( ls_risk_data-puig_amount_purch / ls_risk_data-total_net_revenues )
                                                ELSE 0 ).
                ls_risk_data-puig_dependency  = ls_risk_data-puig_dependency  * 100.

              ENDIF.


              INSERT ls_risk_data INTO TABLE mt_risk_data.

              ls_screening_data-file_contents = VALUE #( FOR <wa1> IN lt_screening_files
                                                    WHERE ( lifnr = ls_screening_data-vendor )
                                                    ( type = <wa1>-type
                                                      content = <wa1>-content ) ).

              INSERT ls_screening_data INTO TABLE mt_screening_data.

            ENDIF.

          ENDLOOP.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD create_alv_data.

    FIELD-SYMBOLS: <lt_alv_data> TYPE STANDARD TABLE.
    FIELD-SYMBOLS: <ls_alv_data> TYPE any.
    FIELD-SYMBOLS: <ls_integration_data_object> TYPE any.
    DATA: lo_abap_typedescr TYPE REF TO cl_abap_typedescr.
    DATA: lo_abap_datadescr TYPE REF TO cl_abap_datadescr.
    DATA: lv_string TYPE string.

*  Se recuperan los componentes de los campos parametrizados
    DATA(lt_components) = mo_db_api_control->create_component_table( ).
    INSERT LINES OF mo_api_screening->create_component_table( iv_only_persis_fields = abap_false ) INTO TABLE lt_components.

*  Se le añaden los campos del log de ejecución
    lo_abap_typedescr ?= cl_abap_datadescr=>describe_by_data( lv_string ).
    lo_abap_datadescr ?= lo_abap_typedescr.
    lt_components = VALUE #( BASE lt_components type = lo_abap_datadescr ( name = mc_fixed_columns-vendor_desc  )
                                                                         ( name = mc_fixed_columns-category1 )
                                                                         ( name = mc_fixed_columns-category1_desc )
                                                                         ( name = mc_fixed_columns-category2 )
                                                                         ( name = mc_fixed_columns-category2_desc )
                                                                         ( name = mc_fixed_columns-category3 )
                                                                         ( name = mc_fixed_columns-category3_desc ) ).
*   Se crea la tabla
    DATA(lo_structdescr) = cl_abap_structdescr=>create( lt_components ).
    DATA(lo_tabledescr) = cl_abap_tabledescr=>create( p_line_type = lo_structdescr ).

    CREATE DATA mo_alv_data_line TYPE HANDLE lo_structdescr.
    CREATE DATA mo_alv_data TYPE HANDLE lo_tabledescr.
    ASSIGN mo_alv_data->* TO <lt_alv_data>.

    LOOP AT mt_supplier_data ASSIGNING FIELD-SYMBOL(<fs_supp>).
      LOOP AT mt_risk_data ASSIGNING FIELD-SYMBOL(<fs_risk_data>)
            WHERE lifnr = <fs_supp>-lifnr.

        ASSIGN mo_alv_data_line->* TO <ls_alv_data>.

*     Se cambia el tipo al icono correspondiente
        ASSIGN COMPONENT 'SUPPLIER' OF STRUCTURE <ls_alv_data> TO FIELD-SYMBOL(<fs_supplier>).
        IF sy-subrc EQ 0.
          <fs_supplier> = <fs_risk_data>-lifnr.
        ENDIF.

        <ls_alv_data> = CORRESPONDING #( BASE ( <ls_alv_data> ) <fs_risk_data> ).

*     Se añaden los datos del proveedor
        <ls_alv_data> = CORRESPONDING #( BASE ( <ls_alv_data> ) <fs_supp> ) .


        INSERT <ls_alv_data> INTO TABLE <lt_alv_data>.
      ENDLOOP.

      LOOP AT mt_screening_data ASSIGNING FIELD-SYMBOL(<ls_screening>) WHERE vendor = <fs_supp>-lifnr.

        " Solo hay un proveedor, por lo tanto se lee la primera linea.
        READ TABLE <lt_alv_data> ASSIGNING <ls_alv_data> INDEX 1.
        IF sy-subrc NE 0.
          INSERT INITIAL LINE INTO TABLE <lt_alv_data> ASSIGNING <ls_alv_data>.

          ASSIGN COMPONENT 'SUPPLIER' OF STRUCTURE <ls_alv_data> TO <fs_supplier>.
          IF sy-subrc EQ 0.
            <fs_supplier> = <fs_risk_data>-lifnr.
          ENDIF.

          <ls_alv_data> = CORRESPONDING #( BASE ( <ls_alv_data> ) <fs_supp> ) .

        ENDIF.

        ASSIGN COMPONENT zcl_db_screening_controller=>cs_api_fields-categories OF STRUCTURE <ls_alv_data> TO FIELD-SYMBOL(<categories>).
        IF sy-subrc = 0.
          <categories> = <ls_screening>-event_categories.

        ENDIF.

        ASSIGN COMPONENT mo_api_screening->cs_api_fields-have_file OF STRUCTURE <ls_alv_data> TO FIELD-SYMBOL(<have_files>).
        IF sy-subrc = 0.
          <have_files> = COND #( WHEN <ls_screening>-file_contents IS NOT INITIAL THEN abap_true ELSE abap_false ).
        ENDIF.

      ENDLOOP.

    ENDLOOP.


  ENDMETHOD.


  METHOD show_alv.
    FIELD-SYMBOLS: <lt_alv_data> TYPE ANY TABLE.

    IF mo_container IS NOT BOUND.
      CREATE OBJECT mo_container
        EXPORTING
          container_name = mc_container_name.
    ENDIF.


    mo_alv = NEW zcl_ca_alv(  ).

    ASSIGN mo_alv_data->* TO <lt_alv_data>.

    CALL METHOD mo_alv->crear_alv
      EXPORTING
        i_programa      = sy-repid
        i_container     = mo_container
      CHANGING
        c_datos         = <lt_alv_data>
      EXCEPTIONS
        error_crear_alv = 1
        OTHERS          = 2.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE w000(zintegra) WITH TEXT-002.
    ENDIF.

*   Indico que las columnas van estar optimizadas
    mo_alv->set_cols_optimizadas( ).


    mo_alv->add_funcion( i_name = cs_alv_functions-update
                         i_icon = icon_refresh
                         i_text = 'Actualizar' ).

    set_alv_columns( ).

    " Eventos del ALV
    SET HANDLER: on_user_command FOR mo_alv,
                   link_click FOR mo_alv.

*   Se muestra el ALV
    mo_alv->mostrar_alv( ).

  ENDMETHOD.

  METHOD set_alv_columns.

    DATA lv_column_position TYPE i VALUE 0.

*   Campos de la parametrización
    DATA(lt_fields) = mo_db_api_control->get_field_custo( ).
    INSERT LINES OF mo_api_screening->get_field_custo( ) INTO TABLE lt_fields.

*   Columnas fijas
    ADD 1 TO lv_column_position.
    mo_alv->set_atributos_campo( i_campo = CONV #( mc_fixed_columns-vendor_desc )
                                 i_texto_todas = CONV #( TEXT-c04 )
                                 i_position = lv_column_position ).

    ADD 1 TO lv_column_position.
    mo_alv->set_atributos_campo( i_campo = CONV #( mc_fixed_columns-category1 )
                                 i_texto_todas = CONV #( TEXT-c05 )
                                 i_position = lv_column_position ).

    ADD 1 TO lv_column_position.
    mo_alv->set_atributos_campo( i_campo = CONV #( mc_fixed_columns-category1_desc )
                                 i_texto_todas = CONV #( TEXT-c06 )
                                 i_position = lv_column_position ).

    ADD 1 TO lv_column_position.
    mo_alv->set_atributos_campo( i_campo = CONV #( mc_fixed_columns-category2 )
                                 i_texto_todas = CONV #( TEXT-c07 )
                                 i_position = lv_column_position ).

    ADD 1 TO lv_column_position.
    mo_alv->set_atributos_campo( i_campo = CONV #( mc_fixed_columns-category2_desc )
                                 i_texto_todas = CONV #( TEXT-c08 )
                                 i_position = lv_column_position ).

    ADD 1 TO lv_column_position.
    mo_alv->set_atributos_campo( i_campo = CONV #( mc_fixed_columns-category3 )
                                 i_texto_todas = CONV #( TEXT-c09 )
                                 i_position = lv_column_position ).

    ADD 1 TO lv_column_position.
    mo_alv->set_atributos_campo( i_campo = CONV #( mc_fixed_columns-category3_desc )
                                 i_texto_todas = CONV #( TEXT-c10 )
                                 i_position = lv_column_position ).


    SORT lt_fields ASCENDING BY sort_order.

*   Se ajustan las columnas a partir de la parametrización
    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_alv_fields>).
      ADD 1 TO lv_column_position.
      mo_alv->set_atributos_campo( i_campo = CONV lvc_fname( <ls_alv_fields>-field  )
                                   i_texto_todas = COND #(  WHEN <ls_alv_fields>-sap_data_element IS INITIAL
                                                                THEN <ls_alv_fields>-description
                                                                ELSE space )
                                   i_position = lv_column_position ).
    ENDLOOP.

    mo_alv->set_atributos_campo( i_campo = CONV #( mo_api_screening->cs_api_fields-have_file )
                                 i_tipo_campo = if_salv_c_cell_type=>checkbox_hotspot
                                  ).

  ENDMETHOD.


  METHOD on_user_command.

    CASE e_salv_function.
      WHEN cs_alv_functions-update.
        update(  ).
    ENDCASE.

  ENDMETHOD.

  METHOD update.

    get_duns_risk_data_from_api( ).

    mo_alv->refrescar_alv( i_refresh_mode = if_salv_c_refresh=>full ).

    save_data( ).

  ENDMETHOD.

  METHOD get_duns_risk_data_from_api.

    FIELD-SYMBOLS: <lt_alv_data> TYPE ANY TABLE.
    FIELD-SYMBOLS: <ls_alv_data> TYPE any.

    CLEAR: mt_risk_data.
    LOOP AT mt_supplier_data ASSIGNING FIELD-SYMBOL(<fs_supp>).
      TRY.

          WAIT UP TO 2 SECONDS.

          "Buscamos el duns
          DATA(lv_dun) = mo_db_api_control->get_duns( EXPORTING iv_nif     = |{ <fs_supp>-stcd1 }|
                                                                iv_name    = <fs_supp>-name1
                                                                iv_country = <fs_supp>-land1 ).

          IF lv_dun IS NOT INITIAL.

            "Obtenemos los datos de riesgo
            DATA(ls_risk_data) = mo_db_api_control->get_risk_data( iv_duns = lv_dun ).

            "Añadimos el proveedor
            ls_risk_data-lifnr = <fs_supp>-lifnr.

            "Obtenemos los campos calculados
            ls_risk_data-solvency = COND #( WHEN ls_risk_data-total_liabilities <> 0 THEN ( ls_risk_data-total_assets / ls_risk_data-total_liabilities )
                                            ELSE 0 ).
            ls_risk_data-solvency = ls_risk_data-solvency * 100.

            ls_risk_data-profitability = COND #( WHEN ls_risk_data-total_net_revenues <> 0 THEN ( ls_risk_data-profit_after_tax / ls_risk_data-total_net_revenues )
                                            ELSE 0 ).
            ls_risk_data-profitability  = ls_risk_data-profitability  * 100.

            "Facturación de puig
            READ TABLE gt_turn ASSIGNING FIELD-SYMBOL(<fs_turn>)
            WITH KEY supplier = <fs_supp>-lifnr.
            IF sy-subrc EQ 0.
              ls_risk_data-puig_amount_purch = <fs_turn>-turnover.

              ls_risk_data-puig_dependency = COND #( WHEN ls_risk_data-total_net_revenues <> 0 THEN ( ls_risk_data-puig_amount_purch / ls_risk_data-total_net_revenues )
                                              ELSE 0 ).
              ls_risk_data-puig_dependency  = ls_risk_data-puig_dependency  * 100.

            ENDIF.

            INSERT ls_risk_data INTO TABLE mt_risk_data.

          ENDIF.

        CATCH zcx_ca_api INTO DATA(lcx_ca_api).
          MESSAGE w000(zintegra) WITH TEXT-002.
      ENDTRY.
    ENDLOOP.


    "Se combina la info a la tabla
    ASSIGN mo_alv_data->* TO <lt_alv_data>.
    CLEAR: <lt_alv_data>.
    LOOP AT mt_supplier_data ASSIGNING <fs_supp>.

      LOOP AT mt_risk_data ASSIGNING FIELD-SYMBOL(<fs_risk_data>)
          WHERE lifnr = <fs_supp>-lifnr.

        ASSIGN mo_alv_data_line->* TO <ls_alv_data>.

        ASSIGN COMPONENT 'SUPPLIER' OF STRUCTURE <ls_alv_data> TO FIELD-SYMBOL(<fs_supplier>).
        IF sy-subrc EQ 0.
          <fs_supplier> = <fs_risk_data>-lifnr.
        ENDIF.

        <ls_alv_data> = CORRESPONDING #( BASE ( <ls_alv_data> ) <fs_risk_data> ).

*     Se añaden los datos del proveedor
        READ TABLE mt_supplier_data ASSIGNING FIELD-SYMBOL(<ls_supplier_data>) WITH KEY vendor = |{ <fs_risk_data>-lifnr ALPHA = IN  }|.
        IF sy-subrc IS INITIAL.
          <ls_alv_data> = CORRESPONDING #( BASE ( <ls_alv_data> ) <ls_supplier_data> ) .
        ENDIF.

        INSERT <ls_alv_data> INTO TABLE <lt_alv_data>.

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD save_data.

    DATA: lr_struc TYPE REF TO cl_abap_structdescr.
    DATA: lt_comp TYPE cl_abap_structdescr=>component_table.
    DATA: ls_comp LIKE LINE OF lt_comp.
    DATA ls_risk_data TYPE zcl_db_api_controller=>ts_risk_final_data.
    FIELD-SYMBOLS: <lt_alv_data> TYPE ANY TABLE.
    ASSIGN mo_alv_data->* TO <lt_alv_data>.

    lr_struc ?= cl_abap_structdescr=>describe_by_data( ls_risk_data ). " Get the description of the data
    lt_comp = lr_struc->get_components( ). "Get the fields of the structure

    DATA: lt_zdun_t001 TYPE STANDARD TABLE OF zdun_t001.
    DATA: lt_zdun_t002 TYPE STANDARD TABLE OF zdun_t002.

    LOOP AT <lt_alv_data> ASSIGNING FIELD-SYMBOL(<fs_alv_data>).

      ASSIGN COMPONENT 'DUNS' OF STRUCTURE <fs_alv_data> TO FIELD-SYMBOL(<fs_duns>).
      IF sy-subrc EQ 0.
        ASSIGN COMPONENT 'SUPPLIER' OF STRUCTURE <fs_alv_data> TO FIELD-SYMBOL(<fs_lifnr>).
        IF sy-subrc EQ 0.
          "Añadimos la info a la tabla de cabecera
          INSERT VALUE #( lifnr = CONV #( <fs_lifnr> )
                          duns = CONV #( <fs_duns> ) ) INTO TABLE lt_zdun_t001.

          "Insertamos el proveedor
          INSERT VALUE #( duns = CONV #( <fs_duns> )
                          field_name = CONV #( 'LIFNR' )
                          value = <fs_lifnr> ) INTO TABLE lt_zdun_t002.
        ENDIF.

      ENDIF.

      "Añadimos los campos
      LOOP AT lt_comp ASSIGNING FIELD-SYMBOL(<fs_comp>).

        ASSIGN COMPONENT <fs_comp>-name OF STRUCTURE <fs_alv_data> TO FIELD-SYMBOL(<fs_field_value>).
        IF sy-subrc EQ 0.
          INSERT VALUE #( duns = CONV #( <fs_duns> )
                          field_name = CONV #( <fs_comp>-name )
                          value = <fs_field_value> ) INTO TABLE lt_zdun_t002.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

    IF lt_zdun_t001 IS NOT INITIAL.
      MODIFY zdun_t001 FROM TABLE lt_zdun_t001.
    ENDIF.
    IF lt_zdun_t002 IS NOT INITIAL.
      MODIFY zdun_t002 FROM TABLE lt_zdun_t002.
    ENDIF.

  ENDMETHOD.

  METHOD link_click.
    FIELD-SYMBOLS: <lt_alv_data> TYPE STANDARD TABLE.

    ASSIGN mo_alv_data->* TO <lt_alv_data>.

    READ TABLE <lt_alv_data> ASSIGNING FIELD-SYMBOL(<ls_alv_data>) INDEX row.
    IF sy-subrc = 0.
        download_files_screening( ).
    ENDIF.
  ENDMETHOD.
  METHOD download_files_screening.
    DATA lt_solix_tab TYPE solix_tab.
    DATA lv_bytecount TYPE i.
    DATA lv_folder TYPE string.

    ASSIGN mt_screening_data[ 1 ] TO FIELD-SYMBOL(<ls_screening>).
    IF sy-subrc = 0.
      IF <ls_screening>-file_contents IS NOT INITIAL.
        cl_gui_frontend_services=>directory_browse(
          EXPORTING
            window_title         = CONV #( TEXT-t01 )
            initial_folder       = 'c:\'
          CHANGING
            selected_folder      = lv_folder
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            not_supported_by_gui = 3
            OTHERS               = 4 ).
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.

          DATA(lv_count) = 1.
          LOOP AT <ls_screening>-file_contents ASSIGNING FIELD-SYMBOL(<ls_file>).

            lt_solix_tab = cl_bcs_convert=>xstring_to_solix( <ls_file>-content ).
            lv_bytecount = xstrlen( <ls_file>-content ).

            " Nombre fichero
            DATA(lv_filename) = |{ lv_folder }\\{ <ls_screening>-vendor ALPHA = OUT }_{ lv_count }|.

            " Extension. Lo que veo es que viene con el "application/pdf"
            DATA(lv_extension) = <ls_file>-type.
            REPLACE FIRST OCCURRENCE OF 'application/' IN lv_extension WITH space.
            lv_filename = |{ lv_filename }.{ lv_extension }|.
            CONDENSE lv_filename NO-GAPS.

            cl_gui_frontend_services=>gui_download(
              EXPORTING
                 bin_filesize              = lv_bytecount
                 filename                  = lv_filename
                 filetype                  = 'BIN'
              CHANGING
                data_tab                  = lt_solix_tab
               EXCEPTIONS
                 file_write_error          = 1
                 no_batch                  = 2
                 gui_refuse_filetransfer   = 3
                 invalid_type              = 4
                 no_authority              = 5
                 unknown_error             = 6
                 header_not_allowed        = 7
                 separator_not_allowed     = 8
                 filesize_not_allowed      = 9
                 header_too_long           = 10
                 dp_error_create           = 11
                 dp_error_send             = 12
                 dp_error_write            = 13
                 unknown_dp_error          = 14
                 access_denied             = 15
                 dp_out_of_memory          = 16
                 disk_full                 = 17
                 dp_timeout                = 18
                 file_not_found            = 19
                 dataprovider_exception    = 20
                 control_flush_error       = 21
                 not_supported_by_gui      = 22
                 error_no_gui              = 23
                 OTHERS                    = 24 ).

            lv_count = lv_count + 1.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS .
