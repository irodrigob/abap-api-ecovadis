CLASS zcx_ecv DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .
    INTERFACES if_t100_dyn_msg .

    CONSTANTS:
      BEGIN OF connection_type_not_conf,
        msgid TYPE symsgid VALUE 'ZECOVADIS',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF connection_type_not_conf .
    CONSTANTS:
      BEGIN OF error_connection_server,
        msgid TYPE symsgid VALUE 'ZECOVADIS',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_connection_server .
    CONSTANTS:
      BEGIN OF error_generic_token_auth,
        msgid TYPE symsgid VALUE 'ZECOVADIS',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_generic_token_auth .
    CONSTANTS:
      BEGIN OF error_token_authentification,
        msgid TYPE symsgid VALUE 'ZECOVADIS',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_token_authentification .
    CONSTANTS:
      BEGIN OF error_method_evdata,
        msgid TYPE symsgid VALUE 'ZECOVADIS',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_method_evdata .
    CONSTANTS:
      BEGIN OF error_mandatory_evid,
        msgid TYPE symsgid VALUE 'ZECOVADIS',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_mandatory_evid .
    CONSTANTS:
      BEGIN OF sap_object_not_found,
        msgid TYPE symsgid VALUE 'ZECOVADIS',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF sap_object_not_found .
    CONSTANTS:
      BEGIN OF error_assigning_evid,
        msgid TYPE symsgid VALUE 'ZECOVADIS',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_assigning_evid .
    CONSTANTS:
      BEGIN OF inconsistent_evid,
        msgid TYPE symsgid VALUE 'ZECOVADIS',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF inconsistent_evid .
    CONSTANTS:
      BEGIN OF dynamic_mapping_error,
        msgid TYPE symsgid VALUE 'ZECOVADIS',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF dynamic_mapping_error .
    CONSTANTS:
      BEGIN OF persistence_error,
        msgid TYPE symsgid VALUE 'ZECOVADIS',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF persistence_error .
    DATA mv_msgv1 TYPE string .
    DATA mv_msgv2 TYPE string .
    DATA mv_msgv3 TYPE string .
    DATA mv_msgv4 TYPE string .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !mv_msgv1 TYPE string OPTIONAL
        !mv_msgv2 TYPE string OPTIONAL
        !mv_msgv3 TYPE string OPTIONAL
        !mv_msgv4 TYPE string OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_ecv IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->mv_msgv1 = mv_msgv1 .
    me->mv_msgv2 = mv_msgv2 .
    me->mv_msgv3 = mv_msgv3 .
    me->mv_msgv4 = mv_msgv4 .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
