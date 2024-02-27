CLASS zcl_ecv_api_downstream DEFINITION
  PUBLIC
  INHERITING FROM zcl_ca_http_services
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: tt_integration_ids          TYPE STANDARD TABLE OF zecv_e_integration_id WITH EMPTY KEY.
    TYPES: tt_ecovadis_ids             TYPE STANDARD TABLE OF zecv_e_ecovadis_id WITH EMPTY KEY.

    TYPES: BEGIN OF ts_reponse_evdata_common,
             ev_supplier_name            TYPE string,
             client_supplier_name        TYPE string,
             parent_company              TYPE string,
             vat_number                  TYPE string,
             tax_number                  TYPE string,
             siret_number                TYPE string,
             active                      TYPE sap_bool,
             evid                        TYPE zecv_e_ecovadis_id,
             city                        TYPE string,
             state                       TYPE string,
             country                     TYPE string,
             address_1                   TYPE string,
             address_2                   TYPE string,
             website                     TYPE string,
             isic_category               TYPE string,
             employee_range              TYPE string,
             size                        TYPE string,
             turnover                    TYPE string,
             risk_country                TYPE sap_bool,
             supplier_contact_first_name TYPE string,
             supplier_contact_last_name  TYPE string,
             supplier_contact_email      TYPE string,
             supplier_contact_phone      TYPE string,
             campaign_name               TYPE string,
             campaign_type               TYPE string,
             rfp_campaign_icon           TYPE string,
             current_stage               TYPE string,
             progress_status             TYPE string,
             sharing_status              TYPE string,
             request_outcome             TYPE string,
             current_stage_code          TYPE int2,
             progress_status_code        TYPE int2,
             sharing_status_code         TYPE int2,
             request_outcome_code        TYPE int2,
             source                      TYPE string,
             launch_date                 TYPE string,
             deadline                    TYPE string,
             declined                    TYPE sap_bool,
             declined_date               TYPE string,
             last_comment                TYPE string,
             comment_date                TYPE string,
             buyer_action                TYPE string,
             specific_comment            TYPE string,
             buyer_last_contacted        TYPE string,
             published_date              TYPE string,
             status_last_update          TYPE string,
             global_score                TYPE int2,
             env_score                   TYPE int2,
             lab_score                   TYPE int2,
             fbp_score                   TYPE int2,
             sup_score                   TYPE int2,
             global_trend                TYPE string,
             env_trend                   TYPE string,
             lab_trend                   TYPE string,
             fbp_trend                   TYPE string,
             sup_trend                   TYPE string,
             scorecard_link              TYPE string,
             expired                     TYPE sap_bool,
             documents_number            TYPE int2,
             scope_change                TYPE sap_bool,
             initial_requested_scope     TYPE string,
             buyer_contact_first_name    TYPE string,
             buyer_contact_last_name     TYPE string,
             buyer_contact_email         TYPE string,
             nb_flags                    TYPE int2,
             nb_client_filters           TYPE int2,
             nb_integration_ids          TYPE int2,
             nb_client_ca                TYPE int2,
             nb_all_ca                   TYPE int2,
             nb_draft_ca                 TYPE int2,
             nb_requested_ca             TYPE int2,
             nb_in_progress_ca           TYPE int2,
             nb_rejected_ca              TYPE int2,
             nb_completed_ca             TYPE int2,
             nb_overdue_ca               TYPE int2,
             nb_no_validation_ca         TYPE int2,
             nb_not_validated_ca         TYPE int2,
             nb_validated_ca             TYPE int2,
             nb_closed_ca                TYPE int2,
             next_deadline               TYPE string,
             last_modification           TYPE string,
             nb_documents                TYPE int2,
           END OF ts_reponse_evdata_common.

    TYPES: BEGIN OF ts_response_evdata_table,
             integration_id TYPE zecv_e_integration_id.
             INCLUDE TYPE ts_reponse_evdata_common.
           TYPES: END OF ts_response_evdata_table.

    TYPES: BEGIN OF ts_response_evdata,
             integration_ids TYPE tt_integration_ids.
             INCLUDE         TYPE ts_reponse_evdata_common.
           TYPES: END OF ts_response_evdata.

    TYPES: tt_response_evdata_table TYPE STANDARD TABLE OF ts_response_evdata_table WITH EMPTY KEY.
    TYPES: tt_response_evdata TYPE STANDARD TABLE OF ts_response_evdata WITH EMPTY KEY.
    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter iv_type | <p class="shorttext synchronized">Tipo de conexión</p>
    "! @parameter iv_auto_connection | <p class="shorttext synchronized">Conexión automática</p>
    METHODS  constructor
      IMPORTING
                iv_langu           TYPE sylangu DEFAULT sy-langu
                iv_type            TYPE zecv_e_server_type
                iv_auto_connection TYPE sap_bool DEFAULT abap_true
      RAISING   zcx_ecv.
    "! <p class="shorttext synchronized">Establece la conexión</p>
    METHODS connect
      RAISING zcx_ecv.
    "! <p class="shorttext synchronized">Recupera datos de ecovadis a partir del integration id</p>
    METHODS get_data_from_integrations_id
      IMPORTING it_integrations_id TYPE tt_integration_ids
      RETURNING VALUE(rt_data)     TYPE tt_response_evdata
      RAISING   zcx_ecv.
    "! <p class="shorttext synchronized">Recupera datos de ecovadis a partir del evid</p>
    METHODS get_data_from_ecovadis_ids
      IMPORTING it_ecovadis_ids TYPE tt_ecovadis_ids
      RETURNING VALUE(rt_data)  TYPE tt_response_evdata
      RAISING   zcx_ecv.

    CLASS-METHODS convert_response_data_to_table
      IMPORTING it_response_data             TYPE tt_response_evdata
      RETURNING VALUE(rt_reponse_table_data) TYPE tt_response_evdata_table.

  PROTECTED SECTION.



    DATA mo_connection TYPE REF TO zcl_ecv_connection.
    DATA mv_langu TYPE sylangu.
    DATA mv_type TYPE zecv_e_server_type.


  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ECV_API_DOWNSTREAM IMPLEMENTATION.


  METHOD connect.
    " La conexión consiste es instanciar la clase encarga de crear la conexion HTTP con el servidor y
    " obtener el token de autentificación
    mo_connection = NEW #( iv_langu = mv_langu
                           iv_type = mv_type
                           iv_auto_connection = abap_true ).

    mo_http_client = mo_connection->get_connection(  ).
    mo_connection->generate_token(  ).
  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    mv_langu = iv_langu.
    mv_type = iv_type.

    IF iv_auto_connection = abap_true.
      connect(  ).
    ENDIF.

  ENDMETHOD.


  METHOD convert_response_data_to_table.

    LOOP AT it_response_data ASSIGNING FIELD-SYMBOL(<ls_response_data>).

      "Por cada integration ID se genera un registro
      LOOP AT <ls_response_data>-integration_ids ASSIGNING FIELD-SYMBOL(<fs_int_id>).

        APPEND INITIAL LINE TO rt_reponse_table_data ASSIGNING FIELD-SYMBOL(<ls_reponse_table_data>).
        <ls_reponse_table_data> = CORRESPONDING #( <ls_response_data>  EXCEPT integration_id ).
*        READ TABLE <ls_response_data>-integration_ids INTO DATA(lv_integration_id) INDEX 1.
*        <ls_reponse_table_data>-integration_id = lv_integration_id.
        <ls_reponse_table_data>-integration_id = <fs_int_id>.
*        CLEAR: lv_integration_id.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_data_from_ecovadis_ids.

    CLEAR: rt_data.

    DATA: lt_response_data TYPE tt_response_evdata.

    CHECK it_ecovadis_ids IS NOT INITIAL.

    "Se conecta de nuevo. Por algun motivo al llamar a dos servicios seguidos sin reiniciar la conexion, no devuelve datos
    connect( ).

    set_header_value( iv_name  = 'Authorization' iv_value = mo_connection->mv_token_authentification ).
    set_request_method( 'GET' ).
    set_content_type( cs_content_type-json ).

    LOOP AT it_ecovadis_ids ASSIGNING FIELD-SYMBOL(<lv_ecovadis_id>).
      DATA(lv_url) = |/{ mo_connection->get_connection_conf(  )-version }/{ zcl_ecv_data=>cs_connection-methods-get_data }|.
      lv_url = |{ lv_url }?f__evid__eq="{ <lv_ecovadis_id> }"|.

      set_request_uri( lv_url ).

      TRY.

          send(  ).
          receive( IMPORTING ev_data = lt_response_data  ).
          APPEND LINES OF lt_response_data TO rt_data.

        CATCH zcx_ca_http_services INTO DATA(lo_excep).
          RAISE EXCEPTION TYPE zcx_ecv
            EXPORTING
              textid   = zcx_ecv=>error_token_authentification
              mv_msgv1 = lo_excep->mv_status_code
              mv_msgv2 = lo_excep->mv_status_text.
      ENDTRY.
    ENDLOOP.



  ENDMETHOD.


  METHOD get_data_from_integrations_id.

    CLEAR: rt_data.

    IF it_integrations_id IS INITIAL.
      EXIT.
    ENDIF.

    "Se conecta de nuevo. Por algun motivo al llamar a dos servicios seguidos sin reiniciar la conexion, no devuelve datos
    connect( ).

    set_header_value( iv_name  = 'Authorization' iv_value = mo_connection->mv_token_authentification ).
    set_request_method( 'GET' ).
    set_content_type( cs_content_type-json ).

    DATA(lv_url) = |/{ mo_connection->get_connection_conf(  )-version }/{ zcl_ecv_data=>cs_connection-methods-get_data }?integration_id=|.

    lv_url = |{ lv_url }{ REDUCE string( INIT values TYPE string
                              FOR <ids> IN it_integrations_id
                              NEXT values = values && COND #( WHEN values IS NOT INITIAL
                                                        THEN |,"{ <ids> }"|
                                                        ELSE |"{ <ids> }"| ) ) }|.

    set_request_uri( lv_url ).

    TRY.

        send(  ).
        receive( IMPORTING ev_data = rt_data  ).

      CATCH zcx_ca_http_services INTO DATA(lo_excep).
        RAISE EXCEPTION TYPE zcx_ecv
          EXPORTING
            textid   = zcx_ecv=>error_token_authentification
            mv_msgv1 = lo_excep->mv_status_code
            mv_msgv2 = lo_excep->mv_status_text.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
