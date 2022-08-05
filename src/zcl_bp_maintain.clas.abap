CLASS zcl_bp_maintain DEFINITION
  PUBLIC
  INHERITING FROM zcl_bp_standard
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_bp_maintain.
    METHODS constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bp_maintain IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).



  ENDMETHOD.


ENDCLASS.
