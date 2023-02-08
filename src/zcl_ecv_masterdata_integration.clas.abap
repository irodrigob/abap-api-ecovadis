"! <p class="shorttext synchronized">Ecovadis - Masterdata integration</p>
CLASS zcl_ecv_masterdata_integration DEFINITION ABSTRACT
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ts_field_customizing,
             description TYPE zecv_e_api_field_descr.
             INCLUDE TYPE zecv_t002.
           TYPES:      END OF ts_field_customizing.

    TYPES: tt_field_customizing TYPE STANDARD TABLE OF ts_field_customizing WITH EMPTY KEY.

    TYPES tt_integration_id TYPE STANDARD TABLE OF zecv_e_integration_id WITH EMPTY KEY.

    TYPES: BEGIN OF ts_integration_data,
             integration_id TYPE zecv_e_integration_id,
             ecovadis_id    TYPE zecv_e_ecovadis_id,
             data           TYPE REF TO data,
           END OF ts_integration_data.
    TYPES: tt_integration_data TYPE STANDARD TABLE OF ts_integration_data WITH EMPTY KEY.


    CLASS-METHODS: "! <p class="shorttext synchronized">Crea la tabla de componentes para los campos parametrizados</p>
      create_component_table
        RETURNING VALUE(rt_components) TYPE cl_abap_structdescr=>component_table
        RAISING   zcx_ecv.

    CLASS-METHODS: "! <p class="shorttext synchronized">Recupera parametrizacion de campos de la API</p>
      get_field_customizing
        IMPORTING iv_only_persistance_fields  TYPE abap_bool DEFAULT abap_true
                  iv_langu                    TYPE langu DEFAULT sy-langu
        RETURNING VALUE(rt_field_customizing) TYPE tt_field_customizing.

    CLASS-METHODS: "! <p class="shorttext synchronized">Recupera una constante</p>
      get_constant
        IMPORTING iv_constant     TYPE brf_constant
        RETURNING VALUE(rv_value) TYPE zecv_e_constant_value.


    METHODS: "! <p class="shorttext synchronized">Lee los datos de ecovadis almacenados en SAP</p>
      read_integration_data ABSTRACT IMPORTING it_integration_id  TYPE tt_integration_id
                   RETURNING VALUE(rt_integration_data) TYPE tt_integration_data
                   RAISING   zcx_ecv.


  PROTECTED SECTION.


    CONSTANTS: BEGIN OF cs_field_types,
                 array   TYPE zecv_e_api_field_type VALUE 'A',
                 object  TYPE zecv_e_api_field_type VALUE 'O',
                 string  TYPE zecv_e_api_field_type VALUE 'S',
                 boolean TYPE zecv_e_api_field_type VALUE 'B',
                 number  TYPE zecv_e_api_field_type VALUE 'N',
               END OF cs_field_types.

    TYPES ts_ecv_data TYPE zcl_ecv_api_downstream=>ts_response_evdata_table.

    TYPES tt_ecv_data TYPE STANDARD TABLE OF ts_ecv_data WITH EMPTY KEY.

    TYPES tt_ecovadis_id TYPE STANDARD TABLE OF zecv_e_ecovadis_id WITH EMPTY KEY.



    TYPES tr_integration_id TYPE RANGE OF zecv_e_integration_id.

    TYPES: BEGIN OF ts_id_mapping,
             ecovadis_id    TYPE zecv_e_ecovadis_id,
             integration_id TYPE zecv_e_integration_id,
           END OF ts_id_mapping.

    TYPES: tt_id_mapping TYPE STANDARD TABLE OF ts_id_mapping WITH EMPTY KEY.


    METHODS: "! <p class="shorttext synchronized">Valida los datos de ecovadi</p>
      validate_integration_data IMPORTING is_integration_data TYPE ts_integration_data
                                RAISING   zcx_ecv.




*   Metodo abstracto a implementar en clase hija
    METHODS: "! <p class="shorttext synchronized">Graba en SAP los datos parametrizados de la API</p>
      persist_integration_data ABSTRACT IMPORTING is_integration_data TYPE ts_integration_data
                                        RAISING   zcx_ecv.

    METHODS: "! <p class="shorttext synchronized">Valida que el objeto de sap exista</p>
      check_sap_object_exists IMPORTING iv_integration_id  TYPE zecv_e_integration_id
                                        iv_include_deleted TYPE abap_bool DEFAULT abap_false
                              RETURNING VALUE(rv_exists)   TYPE abap_bool.

    METHODS: "! <p class="shorttext synchronized">Copia los datos de la api a la estructura dinamica</p>
      fill_integration_data IMPORTING is_ecv_data                TYPE ts_ecv_data
                            RETURNING VALUE(rs_integration_data) TYPE ts_integration_data
                            RAISING   zcx_ecv.

    METHODS: "! <p class="shorttext synchronized">Aplica conversiones a los datos de la api</p>
      format_api_data IMPORTING is_ecv_data        TYPE ts_ecv_data
                      RETURNING VALUE(rs_ecv_data) TYPE ts_ecv_data.

    METHODS: "! <p class="shorttext synchronized">Crea estructura dinamica con los campos parametrizados</p>
      create_dynamic_structure RETURNING VALUE(ro_structdescr) TYPE REF TO cl_abap_structdescr
                               RAISING   zcx_ecv.

    METHODS: "! <p class="shorttext synchronized">Crea tabla dinamica con los campos parametrizados</p>
      create_dynamic_table RETURNING VALUE(ro_tabledescr) TYPE REF TO cl_abap_tabledescr
                           RAISING   zcx_ecv.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ECV_MASTERDATA_INTEGRATION IMPLEMENTATION.


  METHOD check_sap_object_exists.

*   Por defecto el integration_id es un proveedor
    DATA: lr_loevm TYPE RANGE OF loevm.

    DATA(lv_supplier_id) = CONV lifnr( |{ iv_integration_id ALPHA = IN }| ).


    IF iv_include_deleted EQ abap_false.
      lr_loevm = VALUE #( (  sign = 'I' option = 'EQ' low = space ) ).
    ENDIF.

    SELECT SINGLE lifnr
        FROM lfa1
        INTO @DATA(lv_dummy)
        WHERE lifnr EQ @lv_supplier_id AND
              loevm IN @lr_loevm.

    rv_exists = COND #( WHEN sy-subrc IS INITIAL THEN abap_true
                        ELSE abap_false ).

  ENDMETHOD.


  METHOD create_component_table.

    DATA: lv_bool TYPE sap_bool.
    DATA: lv_string TYPE string.
    DATA: lv_float TYPE p LENGTH 16 DECIMALS 2.
    DATA: lv_int TYPE i.

    DATA: lt_components TYPE cl_abap_structdescr=>component_table.
    DATA: lo_abap_typedescr TYPE REF TO cl_abap_typedescr.
    DATA: lo_abap_datadescr TYPE REF TO cl_abap_datadescr.

    TRY.

*       Se recupera la parametrización de campos
        DATA(lt_fields) = get_field_customizing( ).

*       Se monta una estructura con los campos que se necesitan grabar
        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).

*         Si tiene elemento de datos configurado se usa
          IF <ls_field>-sap_data_element IS NOT INITIAL.
            lo_abap_typedescr ?= cl_abap_datadescr=>describe_by_name( <ls_field>-sap_data_element ).
          ELSE.

*           Si no tiene elemento de datos se usa el de la API
            CASE <ls_field>-field_type.
              WHEN cs_field_types-boolean.
                lo_abap_typedescr ?= cl_abap_datadescr=>describe_by_data( lv_bool ).

              WHEN cs_field_types-number.
                lo_abap_typedescr ?= cl_abap_datadescr=>describe_by_data( lv_int ).

              WHEN cs_field_types-string.
                lo_abap_typedescr ?= cl_abap_datadescr=>describe_by_data( lv_string ).

              WHEN OTHERS.
                CONTINUE. "Not supported

            ENDCASE.
          ENDIF.

          lo_abap_datadescr ?= lo_abap_typedescr.

          rt_components = VALUE #(  BASE rt_components ( type = lo_abap_datadescr name = <ls_field>-field  ) ).

        ENDLOOP.

      CATCH cx_root.
        RAISE EXCEPTION TYPE zcx_ecv
          EXPORTING
            textid = zcx_ecv=>dynamic_mapping_error.
    ENDTRY.

  ENDMETHOD.


  METHOD create_dynamic_structure.

    TRY.

        DATA(lt_components) =  zcl_ecv_masterdata_integration=>create_component_table( ).

*       Se instancia la estructura dinamica
        ro_structdescr = cl_abap_structdescr=>create( lt_components ).

      CATCH cx_root.
        RAISE EXCEPTION TYPE zcx_ecv
          EXPORTING
            textid = zcx_ecv=>dynamic_mapping_error.
    ENDTRY.

  ENDMETHOD.


  METHOD create_dynamic_table.

    TRY.

        DATA(lo_strucdescr) = create_dynamic_structure( ).

        ro_tabledescr = cl_abap_tabledescr=>create( p_line_type = lo_strucdescr ).

      CATCH cx_root.
        RAISE EXCEPTION TYPE zcx_ecv
          EXPORTING
            textid = zcx_ecv=>dynamic_mapping_error.
    ENDTRY.

  ENDMETHOD.


  METHOD fill_integration_data.

    FIELD-SYMBOLS: <ls_ecv_data> TYPE any.
    DATA: lo_data TYPE REF TO data.

    DATA(ls_ecv_data) = format_api_data( is_ecv_data ).

*   Genera la estructura dinamica
    DATA(lo_abap_structdescr) = create_dynamic_structure( ).

*   Copia los datos de la estructura de la api a la dinámica
    CREATE DATA lo_data TYPE HANDLE lo_abap_structdescr.
    ASSIGN lo_data->* TO <ls_ecv_data>.

    <ls_ecv_data> = CORRESPONDING #( ls_ecv_data ).

    rs_integration_data = VALUE #( integration_id = ls_ecv_data-integration_id
                                   ecovadis_id = ls_ecv_data-evid
                                   data = lo_data ).

  ENDMETHOD.


  METHOD format_api_data.

    rs_ecv_data = CORRESPONDING #(  is_ecv_data ).

*   Se convierten las fechas a formato SAP
    REPLACE ALL OCCURRENCES OF '-' in  rs_ecv_data-deadline WITH space.
    REPLACE ALL OCCURRENCES OF '-' in  rs_ecv_data-declined_date WITH space.
    REPLACE ALL OCCURRENCES OF '-' in  rs_ecv_data-comment_date WITH space.
    REPLACE ALL OCCURRENCES OF '-' in  rs_ecv_data-buyer_last_contacted WITH space.
    REPLACE ALL OCCURRENCES OF '-' in  rs_ecv_data-published_date WITH space.
    REPLACE ALL OCCURRENCES OF '-' in  rs_ecv_data-comment_date WITH space.
    REPLACE ALL OCCURRENCES OF '-' in  rs_ecv_data-published_date WITH space.
    REPLACE ALL OCCURRENCES OF '-' in  rs_ecv_data-status_last_update WITH space.

  ENDMETHOD.


  METHOD get_constant.

    SELECT SINGLE valor
        FROM zint_t000
        INTO rv_value
        WHERE constante EQ iv_constant.

  ENDMETHOD.


  METHOD get_field_customizing.

    DATA lv_max_sort TYPE zecv_e_api_field_order.
    DATA: lr_persist TYPE RANGE OF zecv_e_api_field_persist.

    IF iv_only_persistance_fields EQ abap_true.
      lr_persist = VALUE #( ( sign = 'I' option = 'EQ' low = iv_only_persistance_fields ) ).
    ENDIF.

    SELECT a~field, a~field_type, a~sap_data_element, a~persist, a~sort_order, b~description
        FROM zecv_t002 AS a LEFT OUTER JOIN zecv_t002t AS b ON a~field EQ b~field AND
                                                               b~spras EQ @iv_langu
        INTO CORRESPONDING FIELDS OF TABLE @rt_field_customizing
        WHERE a~persist  IN @lr_persist.


    CHECK rt_field_customizing IS NOT INITIAL.

*   Para los campos que no tienen un orden se calcula a partir del máximo
    SORT rt_field_customizing DESCENDING BY sort_order.
    READ TABLE rt_field_customizing INTO DATA(ls_higher_field) INDEX 1.
    lv_max_sort = ls_higher_field-sort_order.

    LOOP AT rt_field_customizing ASSIGNING FIELD-SYMBOL(<ls_field>) WHERE sort_order IS INITIAL.
      ADD 1 TO lv_max_sort.
      <ls_field>-sort_order = lv_max_sort.
    ENDLOOP.

    SORT rt_field_customizing ASCENDING BY sort_order.

  ENDMETHOD.


  METHOD validate_integration_data.

*   Datos obligatorios
    IF is_integration_data-integration_id IS INITIAL OR
       is_integration_data-ecovadis_id IS INITIAL.
      RAISE EXCEPTION TYPE zcx_ecv
        EXPORTING
          textid = zcx_ecv=>error_mandatory_evid.
    ENDIF.

    IF check_sap_object_exists( is_integration_data-integration_id ) EQ abap_false.
      RAISE EXCEPTION TYPE zcx_ecv
        EXPORTING
          textid   = zcx_ecv=>sap_object_not_found
          mv_msgv1 = CONV scx_attrname( is_integration_data-integration_id ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
