*&---------------------------------------------------------------------*
*& Include          Z_RELATORIO_TOPOO
*&---------------------------------------------------------------------*
TABLES: ekko, ekpo.


*Bloco Range EBELN

TYPES: BEGIN OF ty_range_ebeln,
         sign   TYPE c LENGTH 1,
         option TYPE c LENGTH 2,
         low    TYPE ekko-ebeln,
         high   TYPE ekko-ebeln,
       END OF ty_range_ebeln.


TYPES: tt_range_ebeln TYPE TABLE OF ty_range_ebeln WITH DEFAULT KEY.

DATA: lt_ranges_ebeln TYPE tt_range_ebeln.


* Bloco Range EBELP

TYPES: BEGIN OF ty_range_ebelp,
         sign   TYPE c LENGTH 1,
         option TYPE c LENGTH 2,
         low    TYPE ekpo-ebelp,
         high   TYPE ekpo-ebelp,
       END OF ty_range_ebelp.

TYPES: tt_range_ebelp TYPE TABLE OF ty_range_ebelp WITH DEFAULT KEY.

DATA: lt_ranges_ebelp TYPE tt_range_ebelp.


* Bloco Range AEDAT

TYPES: BEGIN OF ty_range_aedat,
         sign   TYPE c LENGTH 1,
         option TYPE c LENGTH 2,
         low    TYPE ekko-aedat,
         high   TYPE ekko-aedat,
       END OF ty_range_aedat.

TYPES: tt_range_aedat TYPE TABLE OF ty_range_aedat WITH DEFAULT KEY.

DATA: lt_ranges_aedat TYPE tt_range_aedat.


* Bloco Range ERNAM

TYPES: BEGIN OF ty_range_ernam,
         sign   TYPE c LENGTH 1,
         option TYPE c LENGTH 2,
         low    TYPE ekko-ernam,
         high   TYPE ekko-ernam,
       END OF ty_range_ernam.

TYPES: tt_range_ernam TYPE TABLE OF ty_range_ernam WITH DEFAULT KEY.

DATA: lt_ranges_ernam TYPE tt_range_ernam.
