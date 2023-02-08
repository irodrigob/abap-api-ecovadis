CLASS zcl_ecv_connection DEFINITION
  PUBLIC
  INHERITING FROM zcl_ca_http_services
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_connection_conf,
             type    TYPE zecv_t001-type,
             url     TYPE zecv_t001-url,
             version TYPE zecv_t001-version,
           END OF ts_connection_conf.
    DATA mv_token_authentification TYPE string READ-ONLY.

    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter iv_type | <p class="shorttext synchronized">Tipo de conexión</p>
    "! @parameter iv_auto_connection | <p class="shorttext synchronized">Conexión automática</p>
    METHODS constructor
      IMPORTING
                iv_langu           TYPE sylangu DEFAULT sy-langu
                iv_type            TYPE zecv_e_server_type
                iv_auto_connection TYPE sap_bool DEFAULT abap_false
      RAISING   zcx_ecv.
    "! <p class="shorttext synchronized">Conecta con el servidor</p>
    "! @parameter ro_connection | <p class="shorttext synchronized">Conexión</p>
    METHODS connect
      RETURNING VALUE(ro_connection) TYPE REF TO if_http_client
      RAISING   zcx_ecv.
    "! <p class="shorttext synchronized">Desconexión</p>
    METHODS disconnect.

    "! <p class="shorttext synchronized">Datos de la configuración de la conexión</p>
    "! @parameter ro_connection | <p class="shorttext synchronized">Datos de configuración</p>
    METHODS get_connection_conf
      RETURNING VALUE(rs_configuration) TYPE ts_connection_conf.
    "! <p class="shorttext synchronized">Devuelve la conexión</p>
    "! @parameter ro_connection | <p class="shorttext synchronized">Conexión</p>
    METHODS get_connection
      RETURNING VALUE(ro_connection) TYPE REF TO if_http_client.
    "! <p class="shorttext synchronized">Generación del token de autentificación</p>
    METHODS generate_token
      RAISING zcx_ecv.
  PROTECTED SECTION.
    TYPES: BEGIN OF ts_response_evtoken,
             access_token TYPE string,
             token_type   TYPE string,
           END OF ts_response_evtoken.
    DATA mv_langu TYPE sylangu.
    DATA mv_type TYPE zecv_e_server_type.
    DATA ms_connection TYPE zecv_t001.
    "! <p class="shorttext synchronized">Lectura de la configuración</p>
    METHODS load_configuration
      RAISING zcx_ecv.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ecv_connection IMPLEMENTATION.
  METHOD constructor.
    super->constructor(  ).

    mv_langu = iv_langu.
    mv_type = iv_type.

    load_configuration(  ).

    IF iv_auto_connection = abap_true.
      connect(  ).
    ENDIF.

  ENDMETHOD.

  METHOD load_configuration.

    SELECT SINGLE * INTO ms_connection
           FROM zecv_t001
           WHERE type = mv_type.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_ecv
        EXPORTING
          textid = zcx_ecv=>connection_type_not_conf.
    ENDIF.

  ENDMETHOD.

  METHOD connect.
    CLEAR: ro_connection.
    TRY.
        create_http_client( iv_host = |{ ms_connection-url }| iv_is_https = abap_true ).

        ro_connection = mo_http_client.

      CATCH  zcx_ca_http_services.
        RAISE EXCEPTION TYPE zcx_ecv
          EXPORTING
            textid = zcx_ecv=>error_connection_server.
    ENDTRY.

  ENDMETHOD.
  METHOD disconnect.
    CLEAR: mo_http_client, mv_token_authentification.
  ENDMETHOD.
  METHOD get_connection_conf.
    rs_configuration = CORRESPONDING #( ms_connection ).
  ENDMETHOD.

  METHOD generate_token.
    DATA ls_response TYPE ts_response_evtoken.

    IF mo_http_client IS NOT BOUND.
      connect(  ).
    ENDIF.

    set_request_method( 'POST' ).
    set_content_type( cs_content_type-json ).
    set_request_uri( |/EVToken| ).

    TRY.
        send( iv_data = |grant_type=password&username={ ms_connection-username }&password={ ms_connection-password }| ).

        receive( IMPORTING ev_data = ls_response  ).

        mv_token_authentification = |{ ls_response-token_type } { ls_response-access_token }|.


      CATCH zcx_ca_http_services INTO DATA(lo_excep_http).

        IF lo_excep_http->textid = zcx_ca_http_services=>error_send_data.
          RAISE EXCEPTION TYPE zcx_ecv
            EXPORTING
              textid = zcx_ecv=>error_generic_token_auth.
        ELSE.
          RAISE EXCEPTION TYPE zcx_ecv
            EXPORTING
              textid   = zcx_ecv=>error_token_authentification
              mv_msgv1 = lo_excep_http->mv_status_code
              mv_msgv2 = lo_excep_http->mv_status_text.
        ENDIF.

    ENDTRY.

  ENDMETHOD.



  METHOD get_connection.
    ro_connection = mo_http_client.
  ENDMETHOD.

ENDCLASS.
