/* Generated from analysis/helpers/prelude.scm by the CHICKEN compiler
   http://www.call-cc.org
   Version 5.4.0 (rev 1a1d1495)
   openbsd-unix-clang-x86-64 [ 64bit dload ]
   command line: analysis/helpers/prelude.scm -output-file analysis/helpers/prelude.c -optimize-level 3 -lfa2
   uses: eval data-structures read-syntax extras library
*/
#include "chicken.h"

static C_PTABLE_ENTRY *create_ptable(void);
C_noret_decl(C_eval_toplevel)
C_externimport void C_ccall C_eval_toplevel(C_word c,C_word *av) C_noret;
C_noret_decl(C_data_2dstructures_toplevel)
C_externimport void C_ccall C_data_2dstructures_toplevel(C_word c,C_word *av) C_noret;
C_noret_decl(C_read_2dsyntax_toplevel)
C_externimport void C_ccall C_read_2dsyntax_toplevel(C_word c,C_word *av) C_noret;
C_noret_decl(C_extras_toplevel)
C_externimport void C_ccall C_extras_toplevel(C_word c,C_word *av) C_noret;
C_noret_decl(C_library_toplevel)
C_externimport void C_ccall C_library_toplevel(C_word c,C_word *av) C_noret;

static C_TLS C_word lf[153];
static double C_possibly_force_alignment;
static C_char C_TLS li0[] C_aligned={C_lihdr(0,0,9),40,36,36,32,102,32,207,137,41,0,0,0,0,0,0,0};
static C_char C_TLS li1[] C_aligned={C_lihdr(0,0,6),40,73,32,207,137,41,0,0};
static C_char C_TLS li2[] C_aligned={C_lihdr(0,0,11),40,102,95,49,50,54,51,32,206,177,41,0,0,0,0,0};
static C_char C_TLS li3[] C_aligned={C_lihdr(0,0,6),40,75,32,207,137,41,0,0};
static C_char C_TLS li4[] C_aligned={C_lihdr(0,0,11),40,102,95,49,50,55,48,32,206,177,41,0,0,0,0,0};
static C_char C_TLS li5[] C_aligned={C_lihdr(0,0,11),40,102,95,49,50,54,56,32,207,137,41,0,0,0,0,0};
static C_char C_TLS li6[] C_aligned={C_lihdr(0,0,5),40,67,32,102,41,0,0,0};
static C_char C_TLS li7[] C_aligned={C_lihdr(0,0,11),40,102,95,49,50,55,56,32,206,177,41,0,0,0,0,0};
static C_char C_TLS li8[] C_aligned={C_lihdr(0,0,10),40,68,32,102,32,46,32,207,137,41,0,0,0,0,0,0};
static C_char C_TLS li9[] C_aligned={C_lihdr(0,0,11),40,102,95,49,50,57,52,32,207,137,41,0,0,0,0,0};
static C_char C_TLS li10[] C_aligned={C_lihdr(0,0,7),40,83,32,103,32,102,41,0};
static C_char C_TLS li11[] C_aligned={C_lihdr(0,0,14),40,102,95,49,51,48,54,32,206,177,32,207,137,41,0,0};
static C_char C_TLS li12[] C_aligned={C_lihdr(0,0,8),40,83,83,32,103,32,102,41};
static C_char C_TLS li13[] C_aligned={C_lihdr(0,0,11),40,102,95,49,51,49,56,32,207,137,41,0,0,0,0,0};
static C_char C_TLS li14[] C_aligned={C_lihdr(0,0,9),40,74,32,104,32,103,32,102,41,0,0,0,0,0,0,0};
static C_char C_TLS li15[] C_aligned={C_lihdr(0,0,14),40,102,95,49,51,51,52,32,206,177,32,207,137,41,0,0};
static C_char C_TLS li16[] C_aligned={C_lihdr(0,0,10),40,74,74,32,104,32,103,32,102,41,0,0,0,0,0,0};
static C_char C_TLS li17[] C_aligned={C_lihdr(0,0,14),40,102,95,49,51,53,48,32,206,177,32,207,137,41,0,0};
static C_char C_TLS li18[] C_aligned={C_lihdr(0,0,7),40,79,32,103,32,102,41,0};
static C_char C_TLS li19[] C_aligned={C_lihdr(0,0,17),40,102,50,50,51,54,32,206,177,51,48,48,50,50,51,53,41,0,0,0,0,0,0,0};
static C_char C_TLS li20[] C_aligned={C_lihdr(0,0,17),40,102,50,50,51,51,32,207,137,50,57,56,50,50,51,50,41,0,0,0,0,0,0,0};
static C_char C_TLS li21[] C_aligned={C_lihdr(0,0,11),40,102,95,49,51,54,54,32,207,137,41,0,0,0,0,0};
static C_char C_TLS li22[] C_aligned={C_lihdr(0,0,10),40,38,38,38,32,46,32,102,115,41,0,0,0,0,0,0};
static C_char C_TLS li23[] C_aligned={C_lihdr(0,0,12),40,102,95,49,51,56,53,32,207,137,115,41,0,0,0,0};
static C_char C_TLS li24[] C_aligned={C_lihdr(0,0,10),40,42,42,42,32,46,32,102,115,41,0,0,0,0,0,0};
static C_char C_TLS li25[] C_aligned={C_lihdr(0,0,10),40,116,97,112,32,102,32,207,137,41,0,0,0,0,0,0};
static C_char C_TLS li26[] C_aligned={C_lihdr(0,0,14),40,226,136,167,226,136,167,32,206,177,32,207,137,41,0,0};
static C_char C_TLS li27[] C_aligned={C_lihdr(0,0,14),40,226,136,168,226,136,168,32,206,177,32,207,137,41,0,0};
static C_char C_TLS li28[] C_aligned={C_lihdr(0,0,10),40,226,136,136,32,107,32,207,137,41,0,0,0,0,0,0};
static C_char C_TLS li29[] C_aligned={C_lihdr(0,0,12),40,99,111,110,106,32,206,177,32,207,137,41,0,0,0,0};
static C_char C_TLS li30[] C_aligned={C_lihdr(0,0,11),40,226,134,145,110,32,110,32,207,137,41,0,0,0,0,0};
static C_char C_TLS li31[] C_aligned={C_lihdr(0,0,11),40,226,134,147,110,32,110,32,207,137,41,0,0,0,0,0};
static C_char C_TLS li32[] C_aligned={C_lihdr(0,0,11),40,226,141,139,32,102,32,207,137,115,41,0,0,0,0,0};
static C_char C_TLS li33[] C_aligned={C_lihdr(0,0,13),40,114,105,103,104,116,32,118,97,108,117,101,41,0,0,0};
static C_char C_TLS li34[] C_aligned={C_lihdr(0,0,10),40,114,105,103,104,116,63,32,120,41,0,0,0,0,0,0};
static C_char C_TLS li35[] C_aligned={C_lihdr(0,0,15),40,114,105,103,104,116,45,118,97,108,117,101,32,120,41,0};
static C_char C_TLS li36[] C_aligned={C_lihdr(0,0,12),40,108,101,102,116,32,118,97,108,117,101,41,0,0,0,0};
static C_char C_TLS li37[] C_aligned={C_lihdr(0,0,9),40,108,101,102,116,63,32,120,41,0,0,0,0,0,0,0};
static C_char C_TLS li38[] C_aligned={C_lihdr(0,0,14),40,108,101,102,116,45,118,97,108,117,101,32,120,41,0,0};
static C_char C_TLS li39[] C_aligned={C_lihdr(0,0,13),40,101,105,116,104,101,114,63,32,70,207,137,41,0,0,0};
static C_char C_TLS li40[] C_aligned={C_lihdr(0,0,20),40,101,105,116,104,101,114,45,103,117,97,114,100,32,102,32,70,207,137,41,0,0,0,0};
static C_char C_TLS li41[] C_aligned={C_lihdr(0,0,10),40,103,101,116,116,32,70,207,137,41,0,0,0,0,0,0};
static C_char C_TLS li42[] C_aligned={C_lihdr(0,0,15),40,101,110,115,117,114,101,32,112,32,101,32,207,137,41,0};
static C_char C_TLS li43[] C_aligned={C_lihdr(0,0,13),40,102,109,97,112,112,32,102,32,70,207,137,41,0,0,0};
static C_char C_TLS li44[] C_aligned={C_lihdr(0,0,12),40,102,109,97,112,32,102,32,70,207,137,41,0,0,0,0};
static C_char C_TLS li45[] C_aligned={C_lihdr(0,0,13),40,98,105,110,100,100,32,102,32,70,207,137,41,0,0,0};
static C_char C_TLS li46[] C_aligned={C_lihdr(0,0,12),40,98,105,110,100,32,102,32,70,207,137,41,0,0,0,0};
static C_char C_TLS li47[] C_aligned={C_lihdr(0,0,13),40,102,106,111,105,110,110,32,70,70,207,137,41,0,0,0};
static C_char C_TLS li48[] C_aligned={C_lihdr(0,0,12),40,102,106,111,105,110,32,70,70,207,137,41,0,0,0,0};
static C_char C_TLS li49[] C_aligned={C_lihdr(0,0,17),40,102,50,49,55,54,32,206,177,50,57,51,50,49,55,53,41,0,0,0,0,0,0,0};
static C_char C_TLS li50[] C_aligned={C_lihdr(0,0,15),40,116,104,101,110,110,32,70,206,177,32,70,207,137,41,0};
static C_char C_TLS li51[] C_aligned={C_lihdr(0,0,14),40,116,104,101,110,32,70,206,177,32,70,207,137,41,0,0};
static C_char C_TLS li52[] C_aligned={C_lihdr(0,0,17),40,102,50,49,56,49,32,206,177,50,57,51,50,49,56,48,41,0,0,0,0,0,0,0};
static C_char C_TLS li53[] C_aligned={C_lihdr(0,0,12),40,97,115,115,32,70,206,177,32,207,137,41,0,0,0,0};
static C_char C_TLS li54[] C_aligned={C_lihdr(0,0,17),40,102,50,49,56,54,32,206,177,50,57,51,50,49,56,53,41,0,0,0,0,0,0,0};
static C_char C_TLS li55[] C_aligned={C_lihdr(0,0,11),40,97,115,32,70,206,177,32,207,137,41,0,0,0,0,0};
static C_char C_TLS li56[] C_aligned={C_lihdr(0,0,10),40,97,49,55,50,55,32,207,137,41,0,0,0,0,0,0};
static C_char C_TLS li57[] C_aligned={C_lihdr(0,0,10),40,97,49,55,50,49,32,206,177,41,0,0,0,0,0,0};
static C_char C_TLS li58[] C_aligned={C_lihdr(0,0,18),40,108,105,102,116,50,50,32,102,32,70,206,177,32,70,207,137,41,0,0,0,0,0,0};
static C_char C_TLS li59[] C_aligned={C_lihdr(0,0,10),40,97,49,55,52,53,32,207,137,41,0,0,0,0,0,0};
static C_char C_TLS li60[] C_aligned={C_lihdr(0,0,10),40,97,49,55,51,57,32,206,177,41,0,0,0,0,0,0};
static C_char C_TLS li61[] C_aligned={C_lihdr(0,0,17),40,108,105,102,116,50,32,102,32,70,206,177,32,70,207,137,41,0,0,0,0,0,0,0};
static C_char C_TLS li62[] C_aligned={C_lihdr(0,0,13),40,102,99,111,109,112,111,115,101,101,32,102,41,0,0,0};
static C_char C_TLS li63[] C_aligned={C_lihdr(0,0,12),40,102,99,111,109,112,111,115,101,32,102,41,0,0,0,0};
static C_char C_TLS li64[] C_aligned={C_lihdr(0,0,12),40,107,108,101,105,115,108,105,105,32,102,41,0,0,0,0};
static C_char C_TLS li65[] C_aligned={C_lihdr(0,0,11),40,107,108,101,105,115,108,105,32,102,41,0,0,0,0,0};
static C_char C_TLS li66[] C_aligned={C_lihdr(0,0,20),40,98,114,101,97,107,45,108,101,102,116,32,226,150,179,32,70,207,137,41,0,0,0,0};
static C_char C_TLS li67[] C_aligned={C_lihdr(0,0,14),40,97,49,56,48,50,32,206,177,32,97,99,99,41,0,0};
static C_char C_TLS li68[] C_aligned={C_lihdr(0,0,11),40,97,49,55,57,54,32,226,150,179,41,0,0,0,0,0};
static C_char C_TLS li69[] C_aligned={C_lihdr(0,0,16),40,115,101,113,117,101,110,99,101,101,32,70,207,137,115,41};
static C_char C_TLS li70[] C_aligned={C_lihdr(0,0,14),40,97,49,56,50,56,32,206,177,32,97,99,99,41,0,0};
static C_char C_TLS li71[] C_aligned={C_lihdr(0,0,11),40,97,49,56,50,50,32,226,150,179,41,0,0,0,0,0};
static C_char C_TLS li72[] C_aligned={C_lihdr(0,0,15),40,115,101,113,117,101,110,99,101,32,70,207,137,115,41,0};
static C_char C_TLS li73[] C_aligned={C_lihdr(0,0,18),40,116,114,97,118,101,114,115,101,101,32,102,32,70,207,137,115,41,0,0,0,0,0,0};
static C_char C_TLS li74[] C_aligned={C_lihdr(0,0,17),40,116,114,97,118,101,114,115,101,32,102,32,70,207,137,115,41,0,0,0,0,0,0,0};
static C_char C_TLS li75[] C_aligned={C_lihdr(0,0,13),40,108,109,97,112,112,32,102,32,70,207,137,41,0,0,0};
static C_char C_TLS li76[] C_aligned={C_lihdr(0,0,12),40,108,109,97,112,32,102,32,70,207,137,41,0,0,0,0};
static C_char C_TLS li77[] C_aligned={C_lihdr(0,0,23),40,115,112,108,105,116,45,99,104,111,105,99,101,101,32,103,32,102,32,70,207,137,41,0};
static C_char C_TLS li78[] C_aligned={C_lihdr(0,0,22),40,115,112,108,105,116,45,99,104,111,105,99,101,32,103,32,102,32,70,207,137,41,0,0};
static C_char C_TLS li79[] C_aligned={C_lihdr(0,0,17),40,102,97,110,45,105,110,110,32,103,32,102,32,70,207,137,41,0,0,0,0,0,0,0};
static C_char C_TLS li80[] C_aligned={C_lihdr(0,0,16),40,102,97,110,45,105,110,32,103,32,102,32,70,207,137,41};
static C_char C_TLS li81[] C_aligned={C_lihdr(0,0,12),40,97,49,57,54,48,32,95,32,206,177,41,0,0,0,0};
static C_char C_TLS li82[] C_aligned={C_lihdr(0,0,11),40,97,49,57,53,52,32,226,150,179,41,0,0,0,0,0};
static C_char C_TLS li83[] C_aligned={C_lihdr(0,0,19),40,97,108,116,101,114,110,97,116,105,118,101,32,46,32,70,207,137,41,0,0,0,0,0};
static C_char C_TLS li84[] C_aligned={C_lihdr(0,0,7),40,97,50,48,48,51,41,0};
static C_char C_TLS li85[] C_aligned={C_lihdr(0,0,13),40,97,49,57,57,55,32,101,120,118,97,114,41,0,0,0};
static C_char C_TLS li86[] C_aligned={C_lihdr(0,0,7),40,97,50,48,53,50,41,0};
static C_char C_TLS li87[] C_aligned={C_lihdr(0,0,15),40,116,109,112,50,49,50,48,54,32,97,114,103,115,41,0};
static C_char C_TLS li88[] C_aligned={C_lihdr(0,0,7),40,97,50,48,51,52,41,0};
static C_char C_TLS li89[] C_aligned={C_lihdr(0,0,9),40,97,49,57,57,49,32,107,41,0,0,0,0,0,0,0};
static C_char C_TLS li90[] C_aligned={C_lihdr(0,0,9),40,206,185,32,110,32,207,137,41,0,0,0,0,0,0,0};
static C_char C_TLS li91[] C_aligned={C_lihdr(0,0,12),40,97,50,48,55,49,32,112,111,114,116,41,0,0,0,0};
static C_char C_TLS li92[] C_aligned={C_lihdr(0,0,15),40,97,50,48,56,53,32,207,137,32,112,111,114,116,41,0};
static C_char C_TLS li93[] C_aligned={C_lihdr(0,0,12),40,97,50,49,48,52,32,112,111,114,116,41,0,0,0,0};
static C_char C_TLS li94[] C_aligned={C_lihdr(0,0,15),40,97,50,49,49,56,32,207,137,32,112,111,114,116,41,0};
static C_char C_TLS li95[] C_aligned={C_lihdr(0,0,10),40,116,111,112,108,101,118,101,108,41,0,0,0,0,0,0};


C_noret_decl(f2176)
static void C_ccall f2176(C_word c,C_word *av) C_noret;
C_noret_decl(f2181)
static void C_ccall f2181(C_word c,C_word *av) C_noret;
C_noret_decl(f2186)
static void C_ccall f2186(C_word c,C_word *av) C_noret;
C_noret_decl(f2233)
static void C_fcall f2233(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f2236)
static void C_ccall f2236(C_word c,C_word *av) C_noret;
C_noret_decl(f2246)
static void C_ccall f2246(C_word c,C_word *av) C_noret;
C_noret_decl(f_1211)
static void C_ccall f_1211(C_word c,C_word *av) C_noret;
C_noret_decl(f_1214)
static void C_ccall f_1214(C_word c,C_word *av) C_noret;
C_noret_decl(f_1217)
static void C_ccall f_1217(C_word c,C_word *av) C_noret;
C_noret_decl(f_1220)
static void C_ccall f_1220(C_word c,C_word *av) C_noret;
C_noret_decl(f_1223)
static void C_ccall f_1223(C_word c,C_word *av) C_noret;
C_noret_decl(f_1226)
static void C_ccall f_1226(C_word c,C_word *av) C_noret;
C_noret_decl(f_1244)
static void C_ccall f_1244(C_word c,C_word *av) C_noret;
C_noret_decl(f_1258)
static void C_ccall f_1258(C_word c,C_word *av) C_noret;
C_noret_decl(f_1261)
static void C_ccall f_1261(C_word c,C_word *av) C_noret;
C_noret_decl(f_1263)
static void C_ccall f_1263(C_word c,C_word *av) C_noret;
C_noret_decl(f_1266)
static void C_ccall f_1266(C_word c,C_word *av) C_noret;
C_noret_decl(f_1268)
static void C_ccall f_1268(C_word c,C_word *av) C_noret;
C_noret_decl(f_1270)
static void C_ccall f_1270(C_word c,C_word *av) C_noret;
C_noret_decl(f_1276)
static void C_ccall f_1276(C_word c,C_word *av) C_noret;
C_noret_decl(f_1278)
static void C_ccall f_1278(C_word c,C_word *av) C_noret;
C_noret_decl(f_1286)
static void C_ccall f_1286(C_word c,C_word *av) C_noret;
C_noret_decl(f_1292)
static void C_ccall f_1292(C_word c,C_word *av) C_noret;
C_noret_decl(f_1294)
static void C_ccall f_1294(C_word c,C_word *av) C_noret;
C_noret_decl(f_1302)
static void C_ccall f_1302(C_word c,C_word *av) C_noret;
C_noret_decl(f_1304)
static void C_ccall f_1304(C_word c,C_word *av) C_noret;
C_noret_decl(f_1306)
static void C_ccall f_1306(C_word c,C_word *av) C_noret;
C_noret_decl(f_1314)
static void C_ccall f_1314(C_word c,C_word *av) C_noret;
C_noret_decl(f_1316)
static void C_ccall f_1316(C_word c,C_word *av) C_noret;
C_noret_decl(f_1318)
static void C_ccall f_1318(C_word c,C_word *av) C_noret;
C_noret_decl(f_1326)
static void C_ccall f_1326(C_word c,C_word *av) C_noret;
C_noret_decl(f_1330)
static void C_ccall f_1330(C_word c,C_word *av) C_noret;
C_noret_decl(f_1332)
static void C_ccall f_1332(C_word c,C_word *av) C_noret;
C_noret_decl(f_1334)
static void C_ccall f_1334(C_word c,C_word *av) C_noret;
C_noret_decl(f_1342)
static void C_ccall f_1342(C_word c,C_word *av) C_noret;
C_noret_decl(f_1346)
static void C_ccall f_1346(C_word c,C_word *av) C_noret;
C_noret_decl(f_1348)
static void C_ccall f_1348(C_word c,C_word *av) C_noret;
C_noret_decl(f_1350)
static void C_ccall f_1350(C_word c,C_word *av) C_noret;
C_noret_decl(f_1358)
static void C_ccall f_1358(C_word c,C_word *av) C_noret;
C_noret_decl(f_1362)
static void C_ccall f_1362(C_word c,C_word *av) C_noret;
C_noret_decl(f_1364)
static void C_ccall f_1364(C_word c,C_word *av) C_noret;
C_noret_decl(f_1366)
static void C_ccall f_1366(C_word c,C_word *av) C_noret;
C_noret_decl(f_1381)
static void C_ccall f_1381(C_word c,C_word *av) C_noret;
C_noret_decl(f_1383)
static void C_ccall f_1383(C_word c,C_word *av) C_noret;
C_noret_decl(f_1385)
static void C_ccall f_1385(C_word c,C_word *av) C_noret;
C_noret_decl(f_1393)
static void C_ccall f_1393(C_word c,C_word *av) C_noret;
C_noret_decl(f_1395)
static void C_ccall f_1395(C_word c,C_word *av) C_noret;
C_noret_decl(f_1399)
static void C_ccall f_1399(C_word c,C_word *av) C_noret;
C_noret_decl(f_1401)
static void C_ccall f_1401(C_word c,C_word *av) C_noret;
C_noret_decl(f_1407)
static void C_ccall f_1407(C_word c,C_word *av) C_noret;
C_noret_decl(f_1413)
static void C_ccall f_1413(C_word c,C_word *av) C_noret;
C_noret_decl(f_1417)
static void C_ccall f_1417(C_word c,C_word *av) C_noret;
C_noret_decl(f_1428)
static void C_ccall f_1428(C_word c,C_word *av) C_noret;
C_noret_decl(f_1430)
static void C_ccall f_1430(C_word c,C_word *av) C_noret;
C_noret_decl(f_1441)
static void C_ccall f_1441(C_word c,C_word *av) C_noret;
C_noret_decl(f_1459)
static void C_ccall f_1459(C_word c,C_word *av) C_noret;
C_noret_decl(f_1477)
static void C_ccall f_1477(C_word c,C_word *av) C_noret;
C_noret_decl(f_1483)
static void C_ccall f_1483(C_word c,C_word *av) C_noret;
C_noret_decl(f_1489)
static void C_ccall f_1489(C_word c,C_word *av) C_noret;
C_noret_decl(f_1495)
static void C_ccall f_1495(C_word c,C_word *av) C_noret;
C_noret_decl(f_1505)
static void C_ccall f_1505(C_word c,C_word *av) C_noret;
C_noret_decl(f_1508)
static void C_ccall f_1508(C_word c,C_word *av) C_noret;
C_noret_decl(f_1510)
static void C_ccall f_1510(C_word c,C_word *av) C_noret;
C_noret_decl(f_1516)
static void C_ccall f_1516(C_word c,C_word *av) C_noret;
C_noret_decl(f_1522)
static void C_ccall f_1522(C_word c,C_word *av) C_noret;
C_noret_decl(f_1532)
static void C_ccall f_1532(C_word c,C_word *av) C_noret;
C_noret_decl(f_1535)
static void C_ccall f_1535(C_word c,C_word *av) C_noret;
C_noret_decl(f_1541)
static void C_ccall f_1541(C_word c,C_word *av) C_noret;
C_noret_decl(f_1545)
static void C_ccall f_1545(C_word c,C_word *av) C_noret;
C_noret_decl(f_1553)
static void C_ccall f_1553(C_word c,C_word *av) C_noret;
C_noret_decl(f_1560)
static void C_ccall f_1560(C_word c,C_word *av) C_noret;
C_noret_decl(f_1568)
static void C_ccall f_1568(C_word c,C_word *av) C_noret;
C_noret_decl(f_1575)
static void C_ccall f_1575(C_word c,C_word *av) C_noret;
C_noret_decl(f_1585)
static void C_ccall f_1585(C_word c,C_word *av) C_noret;
C_noret_decl(f_1589)
static void C_ccall f_1589(C_word c,C_word *av) C_noret;
C_noret_decl(f_1591)
static void C_ccall f_1591(C_word c,C_word *av) C_noret;
C_noret_decl(f_1603)
static void C_ccall f_1603(C_word c,C_word *av) C_noret;
C_noret_decl(f_1610)
static void C_ccall f_1610(C_word c,C_word *av) C_noret;
C_noret_decl(f_1617)
static void C_ccall f_1617(C_word c,C_word *av) C_noret;
C_noret_decl(f_1621)
static void C_ccall f_1621(C_word c,C_word *av) C_noret;
C_noret_decl(f_1623)
static void C_ccall f_1623(C_word c,C_word *av) C_noret;
C_noret_decl(f_1631)
static void C_ccall f_1631(C_word c,C_word *av) C_noret;
C_noret_decl(f_1633)
static void C_ccall f_1633(C_word c,C_word *av) C_noret;
C_noret_decl(f_1640)
static void C_ccall f_1640(C_word c,C_word *av) C_noret;
C_noret_decl(f_1647)
static void C_ccall f_1647(C_word c,C_word *av) C_noret;
C_noret_decl(f_1649)
static void C_ccall f_1649(C_word c,C_word *av) C_noret;
C_noret_decl(f_1657)
static void C_ccall f_1657(C_word c,C_word *av) C_noret;
C_noret_decl(f_1659)
static void C_ccall f_1659(C_word c,C_word *av) C_noret;
C_noret_decl(f_1665)
static void C_ccall f_1665(C_word c,C_word *av) C_noret;
C_noret_decl(f_1674)
static void C_ccall f_1674(C_word c,C_word *av) C_noret;
C_noret_decl(f_1684)
static void C_ccall f_1684(C_word c,C_word *av) C_noret;
C_noret_decl(f_1692)
static void C_ccall f_1692(C_word c,C_word *av) C_noret;
C_noret_decl(f_1694)
static void C_ccall f_1694(C_word c,C_word *av) C_noret;
C_noret_decl(f_1704)
static void C_ccall f_1704(C_word c,C_word *av) C_noret;
C_noret_decl(f_1716)
static void C_ccall f_1716(C_word c,C_word *av) C_noret;
C_noret_decl(f_1722)
static void C_ccall f_1722(C_word c,C_word *av) C_noret;
C_noret_decl(f_1728)
static void C_ccall f_1728(C_word c,C_word *av) C_noret;
C_noret_decl(f_1734)
static void C_ccall f_1734(C_word c,C_word *av) C_noret;
C_noret_decl(f_1740)
static void C_ccall f_1740(C_word c,C_word *av) C_noret;
C_noret_decl(f_1746)
static void C_ccall f_1746(C_word c,C_word *av) C_noret;
C_noret_decl(f_1752)
static void C_ccall f_1752(C_word c,C_word *av) C_noret;
C_noret_decl(f_1758)
static void C_ccall f_1758(C_word c,C_word *av) C_noret;
C_noret_decl(f_1764)
static void C_ccall f_1764(C_word c,C_word *av) C_noret;
C_noret_decl(f_1770)
static void C_ccall f_1770(C_word c,C_word *av) C_noret;
C_noret_decl(f_1778)
static void C_ccall f_1778(C_word c,C_word *av) C_noret;
C_noret_decl(f_1785)
static void C_ccall f_1785(C_word c,C_word *av) C_noret;
C_noret_decl(f_1791)
static void C_ccall f_1791(C_word c,C_word *av) C_noret;
C_noret_decl(f_1797)
static void C_ccall f_1797(C_word c,C_word *av) C_noret;
C_noret_decl(f_1803)
static void C_ccall f_1803(C_word c,C_word *av) C_noret;
C_noret_decl(f_1811)
static void C_ccall f_1811(C_word c,C_word *av) C_noret;
C_noret_decl(f_1817)
static void C_ccall f_1817(C_word c,C_word *av) C_noret;
C_noret_decl(f_1823)
static void C_ccall f_1823(C_word c,C_word *av) C_noret;
C_noret_decl(f_1829)
static void C_ccall f_1829(C_word c,C_word *av) C_noret;
C_noret_decl(f_1837)
static void C_ccall f_1837(C_word c,C_word *av) C_noret;
C_noret_decl(f_1843)
static void C_ccall f_1843(C_word c,C_word *av) C_noret;
C_noret_decl(f_1851)
static void C_ccall f_1851(C_word c,C_word *av) C_noret;
C_noret_decl(f_1853)
static void C_ccall f_1853(C_word c,C_word *av) C_noret;
C_noret_decl(f_1861)
static void C_ccall f_1861(C_word c,C_word *av) C_noret;
C_noret_decl(f_1863)
static void C_ccall f_1863(C_word c,C_word *av) C_noret;
C_noret_decl(f_1870)
static void C_ccall f_1870(C_word c,C_word *av) C_noret;
C_noret_decl(f_1877)
static void C_ccall f_1877(C_word c,C_word *av) C_noret;
C_noret_decl(f_1881)
static void C_ccall f_1881(C_word c,C_word *av) C_noret;
C_noret_decl(f_1883)
static void C_ccall f_1883(C_word c,C_word *av) C_noret;
C_noret_decl(f_1891)
static void C_ccall f_1891(C_word c,C_word *av) C_noret;
C_noret_decl(f_1894)
static void C_ccall f_1894(C_word c,C_word *av) C_noret;
C_noret_decl(f_1902)
static void C_ccall f_1902(C_word c,C_word *av) C_noret;
C_noret_decl(f_1904)
static void C_ccall f_1904(C_word c,C_word *av) C_noret;
C_noret_decl(f_1912)
static void C_ccall f_1912(C_word c,C_word *av) C_noret;
C_noret_decl(f_1914)
static void C_ccall f_1914(C_word c,C_word *av) C_noret;
C_noret_decl(f_1921)
static void C_ccall f_1921(C_word c,C_word *av) C_noret;
C_noret_decl(f_1928)
static void C_ccall f_1928(C_word c,C_word *av) C_noret;
C_noret_decl(f_1935)
static void C_ccall f_1935(C_word c,C_word *av) C_noret;
C_noret_decl(f_1937)
static void C_ccall f_1937(C_word c,C_word *av) C_noret;
C_noret_decl(f_1945)
static void C_ccall f_1945(C_word c,C_word *av) C_noret;
C_noret_decl(f_1949)
static void C_ccall f_1949(C_word c,C_word *av) C_noret;
C_noret_decl(f_1955)
static void C_ccall f_1955(C_word c,C_word *av) C_noret;
C_noret_decl(f_1961)
static void C_ccall f_1961(C_word c,C_word *av) C_noret;
C_noret_decl(f_1968)
static void C_ccall f_1968(C_word c,C_word *av) C_noret;
C_noret_decl(f_1974)
static void C_ccall f_1974(C_word c,C_word *av) C_noret;
C_noret_decl(f_1978)
static void C_ccall f_1978(C_word c,C_word *av) C_noret;
C_noret_decl(f_1981)
static void C_ccall f_1981(C_word c,C_word *av) C_noret;
C_noret_decl(f_1992)
static void C_ccall f_1992(C_word c,C_word *av) C_noret;
C_noret_decl(f_1998)
static void C_ccall f_1998(C_word c,C_word *av) C_noret;
C_noret_decl(f_2004)
static void C_ccall f_2004(C_word c,C_word *av) C_noret;
C_noret_decl(f_2021)
static void C_ccall f_2021(C_word c,C_word *av) C_noret;
C_noret_decl(f_2035)
static void C_ccall f_2035(C_word c,C_word *av) C_noret;
C_noret_decl(f_2037)
static C_word C_fcall f_2037(C_word t0);
C_noret_decl(f_2039)
static C_word C_fcall f_2039(C_word t0);
C_noret_decl(f_2047)
static void C_fcall f_2047(C_word t0,C_word t1,C_word t2) C_noret;
C_noret_decl(f_2053)
static void C_ccall f_2053(C_word c,C_word *av) C_noret;
C_noret_decl(f_2070)
static void C_ccall f_2070(C_word c,C_word *av) C_noret;
C_noret_decl(f_2072)
static void C_ccall f_2072(C_word c,C_word *av) C_noret;
C_noret_decl(f_2084)
static void C_ccall f_2084(C_word c,C_word *av) C_noret;
C_noret_decl(f_2086)
static void C_ccall f_2086(C_word c,C_word *av) C_noret;
C_noret_decl(f_2093)
static void C_ccall f_2093(C_word c,C_word *av) C_noret;
C_noret_decl(f_2096)
static void C_ccall f_2096(C_word c,C_word *av) C_noret;
C_noret_decl(f_2105)
static void C_ccall f_2105(C_word c,C_word *av) C_noret;
C_noret_decl(f_2117)
static void C_ccall f_2117(C_word c,C_word *av) C_noret;
C_noret_decl(f_2119)
static void C_ccall f_2119(C_word c,C_word *av) C_noret;
C_noret_decl(f_2126)
static void C_ccall f_2126(C_word c,C_word *av) C_noret;
C_noret_decl(f_2129)
static void C_ccall f_2129(C_word c,C_word *av) C_noret;
C_noret_decl(C_toplevel)
C_externexport void C_ccall C_toplevel(C_word c,C_word *av) C_noret;

C_noret_decl(trf2233)
static void C_ccall trf2233(C_word c,C_word *av) C_noret;
static void C_ccall trf2233(C_word c,C_word *av){
C_word t0=av[2];
C_word t1=av[1];
C_word t2=av[0];
f2233(t0,t1,t2);}

C_noret_decl(trf_2047)
static void C_ccall trf_2047(C_word c,C_word *av) C_noret;
static void C_ccall trf_2047(C_word c,C_word *av){
C_word t0=av[2];
C_word t1=av[1];
C_word t2=av[0];
f_2047(t0,t1,t2);}

/* f2176 in thenn in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f2176(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f2176,c,av);}
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=((C_word*)t0)[2];
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* f2181 in ass in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f2181(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f2181,c,av);}
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=((C_word*)t0)[2];
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* f2186 in as in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f2186(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f2186,c,av);}
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=((C_word*)t0)[2];
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* f2233 in k1379 */
static void C_fcall f2233(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,0,3)))){
C_save_and_reclaim_args((void *)trf2233,3,t0,t1,t2);}
a=C_alloc(5);
t3=t1;{
C_word av2[2];
av2[0]=t3;
av2[1]=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f2236,a[2]=((C_word*)t0)[2],a[3]=t2,a[4]=((C_word)li19),tmp=(C_word)a,a+=5,tmp);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* f2236 */
static void C_ccall f2236(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f2236,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:19: f"));
t3=((C_word*)t0)[2];{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t3;
av2[1]=t1;
av2[2]=((C_word*)t0)[3];
av2[3]=t2;
((C_proc)C_fast_retrieve_proc(t3))(4,av2);}}

/* f2246 in k1379 */
static void C_ccall f2246(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f2246,c,av);}
C_trace(C_text("##sys#map"));
t2=*((C_word*)lf[59]+1);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
av2[3]=((C_word*)t0)[3];
((C_proc)(void*)(*((C_word*)t2+1)))(4,av2);}}

/* k1209 */
static void C_ccall f_1211(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_1211,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1214,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);{
C_word *av2=av;
av2[0]=C_SCHEME_UNDEFINED;
av2[1]=t2;
C_eval_toplevel(2,av2);}}

/* k1212 in k1209 */
static void C_ccall f_1214(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_1214,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1217,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);{
C_word *av2=av;
av2[0]=C_SCHEME_UNDEFINED;
av2[1]=t2;
C_extras_toplevel(2,av2);}}

/* k1215 in k1212 in k1209 */
static void C_ccall f_1217(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_1217,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1220,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);{
C_word *av2=av;
av2[0]=C_SCHEME_UNDEFINED;
av2[1]=t2;
C_read_2dsyntax_toplevel(2,av2);}}

/* k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1220(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_1220,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1223,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);{
C_word *av2=av;
av2[0]=C_SCHEME_UNDEFINED;
av2[1]=t2;
C_data_2dstructures_toplevel(2,av2);}}

/* k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1223(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,4)))){
C_save_and_reclaim((void *)f_1223,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1226,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:1: chicken.load#load-extension"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[151]);
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[151]+1);
av2[1]=t2;
av2[2]=lf[152];
av2[3]=C_SCHEME_TRUE;
av2[4]=C_SCHEME_FALSE;
tp(5,av2);}}

/* k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1226(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word t13;
C_word t14;
C_word t15;
C_word t16;
C_word t17;
C_word t18;
C_word t19;
C_word t20;
C_word t21;
C_word t22;
C_word t23;
C_word t24;
C_word t25;
C_word t26;
C_word t27;
C_word t28;
C_word t29;
C_word t30;
C_word t31;
C_word t32;
C_word t33;
C_word t34;
C_word t35;
C_word t36;
C_word t37;
C_word t38;
C_word t39;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(39,c,5)))){
C_save_and_reclaim((void *)f_1226,c,av);}
a=C_alloc(39);
t2=C_mutate((C_word*)lf[0]+1 /* (set! ↑ ...) */,*((C_word*)lf[1]+1));
t3=C_mutate((C_word*)lf[2]+1 /* (set! ↓ ...) */,*((C_word*)lf[3]+1));
t4=C_mutate((C_word*)lf[4]+1 /* (set! ↑↓ ...) */,*((C_word*)lf[5]+1));
t5=C_mutate((C_word*)lf[6]+1 /* (set! ↑↑ ...) */,*((C_word*)lf[7]+1));
t6=C_mutate((C_word*)lf[8]+1 /* (set! ↓↓ ...) */,*((C_word*)lf[9]+1));
t7=C_mutate((C_word*)lf[10]+1 /* (set! ∘ ...) */,C_fast_retrieve(lf[11]));
t8=C_mutate((C_word*)lf[12]+1 /* (set! ≡ ...) */,*((C_word*)lf[13]+1));
t9=C_set_block_item(lf[14] /* ∅ */,0,C_SCHEME_END_OF_LIST);
t10=C_mutate((C_word*)lf[15]+1 /* (set! ∅? ...) */,*((C_word*)lf[16]+1));
t11=C_mutate((C_word*)lf[17]+1 /* (set! ρ ...) */,*((C_word*)lf[18]+1));
t12=C_mutate((C_word*)lf[19]+1 /* (set! ρs ...) */,*((C_word*)lf[20]+1));
t13=C_mutate((C_word*)lf[21]+1 /* (set! ◇ ...) */,C_fast_retrieve(lf[22]));
t14=C_mutate((C_word*)lf[23]+1 /* (set! ⊂ ...) */,*((C_word*)lf[24]+1));
t15=C_mutate((C_word*)lf[25]+1 /* (set! ∀ ...) */,*((C_word*)lf[26]+1));
t16=C_mutate((C_word*)lf[27]+1 /* (set! $ ...) */,*((C_word*)lf[28]+1));
t17=C_mutate((C_word*)lf[29]+1 /* (set! ⊖ ...) */,*((C_word*)lf[30]+1));
t18=C_mutate((C_word*)lf[31]+1 /* (set! $$ ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1244,a[2]=((C_word)li0),tmp=(C_word)a,a+=3,tmp));
t19=C_mutate((C_word*)lf[32]+1 /* (set! ⇒ ...) */,*((C_word*)lf[33]+1));
t20=C_mutate((C_word*)lf[34]+1 /* (set! ⇐ ...) */,*((C_word*)lf[35]+1));
t21=C_mutate((C_word*)lf[36]+1 /* (set! ¬ ...) */,*((C_word*)lf[37]+1));
t22=C_mutate((C_word*)lf[38]+1 /* (set! ⍨ ...) */,C_fast_retrieve(lf[39]));
t23=C_mutate((C_word*)lf[40]+1 /* (set! ∀∀ ...) */,*((C_word*)lf[41]+1));
t24=C_mutate((C_word*)lf[42]+1 /* (set! ∞ ...) */,lf[43]);
t25=C_mutate((C_word*)lf[44]+1 /* (set! ⊆ ...) */,*((C_word*)lf[45]+1));
t26=C_mutate((C_word*)lf[46]+1 /* (set! ∀? ...) */,C_fast_retrieve(lf[47]));
t27=C_mutate((C_word*)lf[48]+1 /* (set! I ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1258,a[2]=((C_word)li1),tmp=(C_word)a,a+=3,tmp));
t28=C_mutate((C_word*)lf[49]+1 /* (set! K ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1261,a[2]=((C_word)li3),tmp=(C_word)a,a+=3,tmp));
t29=C_mutate((C_word*)lf[50]+1 /* (set! C ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1266,a[2]=((C_word)li6),tmp=(C_word)a,a+=3,tmp));
t30=C_mutate((C_word*)lf[51]+1 /* (set! D ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1276,a[2]=((C_word)li8),tmp=(C_word)a,a+=3,tmp));
t31=C_mutate((C_word*)lf[53]+1 /* (set! S ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1292,a[2]=((C_word)li10),tmp=(C_word)a,a+=3,tmp));
t32=C_mutate((C_word*)lf[54]+1 /* (set! SS ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1304,a[2]=((C_word)li12),tmp=(C_word)a,a+=3,tmp));
t33=C_mutate((C_word*)lf[55]+1 /* (set! J ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1316,a[2]=((C_word)li14),tmp=(C_word)a,a+=3,tmp));
t34=C_mutate((C_word*)lf[56]+1 /* (set! JJ ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1332,a[2]=((C_word)li16),tmp=(C_word)a,a+=3,tmp));
t35=C_mutate((C_word*)lf[57]+1 /* (set! O ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1348,a[2]=((C_word)li18),tmp=(C_word)a,a+=3,tmp));
t36=C_mutate((C_word*)lf[58]+1 /* (set! &&& ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1364,a[2]=((C_word)li22),tmp=(C_word)a,a+=3,tmp));
t37=C_mutate((C_word*)lf[60]+1 /* (set! *** ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1383,a[2]=((C_word)li24),tmp=(C_word)a,a+=3,tmp));
t38=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1393,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:28: ⍨"));
t39=C_fast_retrieve(lf[38]);{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t39;
av2[1]=t38;
av2[2]=C_fast_retrieve(lf[31]);
((C_proc)(void*)(*((C_word*)t39+1)))(3,av2);}}

/* $$ in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1244(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_1244,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:14: f"));
t4=t2;{
C_word *av2=av;
av2[0]=t4;
av2[1]=t1;
av2[2]=t3;
((C_proc)C_fast_retrieve_proc(t4))(3,av2);}}

/* I in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1258(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_1258,c,av);}
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=t2;
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* K in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1261(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_1261,c,av);}
a=C_alloc(4);
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1263,a[2]=t2,a[3]=((C_word)li2),tmp=(C_word)a,a+=4,tmp);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* f_1263 in K in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1263(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_1263,c,av);}
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=((C_word*)t0)[2];
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* C in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1266(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_1266,c,av);}
a=C_alloc(4);
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1268,a[2]=t2,a[3]=((C_word)li5),tmp=(C_word)a,a+=4,tmp);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* f_1268 in C in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1268(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,3)))){
C_save_and_reclaim((void *)f_1268,c,av);}
a=C_alloc(5);
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1270,a[2]=((C_word*)t0)[2],a[3]=t2,a[4]=((C_word)li4),tmp=(C_word)a,a+=5,tmp);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* f_1270 */
static void C_ccall f_1270(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1270,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:19: f"));
t3=((C_word*)t0)[2];{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t3;
av2[1]=t1;
av2[2]=((C_word*)t0)[3];
av2[3]=t2;
((C_proc)C_fast_retrieve_proc(t3))(4,av2);}}

/* D in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1276(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c<3) C_bad_min_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand((c-3)*C_SIZEOF_PAIR +5,c,3)))){
C_save_and_reclaim((void*)f_1276,c,av);}
a=C_alloc((c-3)*C_SIZEOF_PAIR+5);
t3=C_build_rest(&a,c,3,av);
C_word t4;
t4=t1;{
C_word *av2=av;
av2[0]=t4;
av2[1]=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1278,a[2]=t2,a[3]=t3,a[4]=((C_word)li7),tmp=(C_word)a,a+=5,tmp);
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}

/* f_1278 in D in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1278(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word t5;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,3)))){
C_save_and_reclaim((void *)f_1278,c,av);}
a=C_alloc(7);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1286,a[2]=t1,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
t4=C_a_i_list(&a,1,t2);
C_trace(C_text("analysis/helpers/prelude.scm:20: ##sys#append"));
t5=*((C_word*)lf[52]+1);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t5;
av2[1]=t3;
av2[2]=((C_word*)t0)[3];
av2[3]=t4;
((C_proc)(void*)(*((C_word*)t5+1)))(4,av2);}}

/* k1284 */
static void C_ccall f_1286(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1286,c,av);}{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=0;
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
C_apply(4,av2);}}

/* S in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1292(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,3)))){
C_save_and_reclaim((void *)f_1292,c,av);}
a=C_alloc(5);
t4=t1;{
C_word *av2=av;
av2[0]=t4;
av2[1]=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1294,a[2]=t2,a[3]=t3,a[4]=((C_word)li9),tmp=(C_word)a,a+=5,tmp);
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}

/* f_1294 in S in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1294(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,2)))){
C_save_and_reclaim((void *)f_1294,c,av);}
a=C_alloc(5);
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1302,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=t2,tmp=(C_word)a,a+=5,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:21: f"));
t4=((C_word*)t0)[3];{
C_word *av2=av;
av2[0]=t4;
av2[1]=t3;
av2[2]=t2;
((C_proc)C_fast_retrieve_proc(t4))(3,av2);}}

/* k1300 */
static void C_ccall f_1302(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1302,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:21: g"));
t2=((C_word*)t0)[2];{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[3];
av2[2]=((C_word*)t0)[4];
av2[3]=t1;
((C_proc)C_fast_retrieve_proc(t2))(4,av2);}}

/* SS in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1304(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,4)))){
C_save_and_reclaim((void *)f_1304,c,av);}
a=C_alloc(5);
t4=t1;{
C_word *av2=av;
av2[0]=t4;
av2[1]=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1306,a[2]=t2,a[3]=t3,a[4]=((C_word)li11),tmp=(C_word)a,a+=5,tmp);
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}

/* f_1306 in SS in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1306(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,2)))){
C_save_and_reclaim((void *)f_1306,c,av);}
a=C_alloc(5);
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1314,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=t2,tmp=(C_word)a,a+=5,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:22: f"));
t5=((C_word*)t0)[3];{
C_word *av2=av;
av2[0]=t5;
av2[1]=t4;
av2[2]=t3;
((C_proc)C_fast_retrieve_proc(t5))(3,av2);}}

/* k1312 */
static void C_ccall f_1314(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1314,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:22: g"));
t2=((C_word*)t0)[2];{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[3];
av2[2]=((C_word*)t0)[4];
av2[3]=t1;
((C_proc)C_fast_retrieve_proc(t2))(4,av2);}}

/* J in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1316(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4=av[4];
C_word t5;
C_word *a;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,3)))){
C_save_and_reclaim((void *)f_1316,c,av);}
a=C_alloc(6);
t5=t1;{
C_word *av2=av;
av2[0]=t5;
av2[1]=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1318,a[2]=t2,a[3]=t4,a[4]=t3,a[5]=((C_word)li13),tmp=(C_word)a,a+=6,tmp);
((C_proc)(void*)(*((C_word*)t5+1)))(2,av2);}}

/* f_1318 in J in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1318(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,2)))){
C_save_and_reclaim((void *)f_1318,c,av);}
a=C_alloc(6);
t3=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1326,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=((C_word*)t0)[3],a[5]=t2,tmp=(C_word)a,a+=6,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:23: g"));
t4=((C_word*)t0)[4];{
C_word *av2=av;
av2[0]=t4;
av2[1]=t3;
av2[2]=t2;
((C_proc)C_fast_retrieve_proc(t4))(3,av2);}}

/* k1324 */
static void C_ccall f_1326(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,2)))){
C_save_and_reclaim((void *)f_1326,c,av);}
a=C_alloc(5);
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1330,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t1,tmp=(C_word)a,a+=5,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:23: f"));
t3=((C_word*)t0)[4];{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t3;
av2[1]=t2;
av2[2]=((C_word*)t0)[5];
((C_proc)C_fast_retrieve_proc(t3))(3,av2);}}

/* k1328 in k1324 */
static void C_ccall f_1330(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1330,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:23: h"));
t2=((C_word*)t0)[2];{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[3];
av2[2]=((C_word*)t0)[4];
av2[3]=t1;
((C_proc)C_fast_retrieve_proc(t2))(4,av2);}}

/* JJ in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1332(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4=av[4];
C_word t5;
C_word *a;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,4)))){
C_save_and_reclaim((void *)f_1332,c,av);}
a=C_alloc(6);
t5=t1;{
C_word *av2=av;
av2[0]=t5;
av2[1]=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1334,a[2]=t2,a[3]=t4,a[4]=t3,a[5]=((C_word)li15),tmp=(C_word)a,a+=6,tmp);
((C_proc)(void*)(*((C_word*)t5+1)))(2,av2);}}

/* f_1334 in JJ in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1334(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,2)))){
C_save_and_reclaim((void *)f_1334,c,av);}
a=C_alloc(6);
t4=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1342,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=((C_word*)t0)[3],a[5]=t3,tmp=(C_word)a,a+=6,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:24: g"));
t5=((C_word*)t0)[4];{
C_word *av2=av;
av2[0]=t5;
av2[1]=t4;
av2[2]=t2;
((C_proc)C_fast_retrieve_proc(t5))(3,av2);}}

/* k1340 */
static void C_ccall f_1342(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,2)))){
C_save_and_reclaim((void *)f_1342,c,av);}
a=C_alloc(5);
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1346,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t1,tmp=(C_word)a,a+=5,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:24: f"));
t3=((C_word*)t0)[4];{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t3;
av2[1]=t2;
av2[2]=((C_word*)t0)[5];
((C_proc)C_fast_retrieve_proc(t3))(3,av2);}}

/* k1344 in k1340 */
static void C_ccall f_1346(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1346,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:24: h"));
t2=((C_word*)t0)[2];{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[3];
av2[2]=((C_word*)t0)[4];
av2[3]=t1;
((C_proc)C_fast_retrieve_proc(t2))(4,av2);}}

/* O in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1348(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,4)))){
C_save_and_reclaim((void *)f_1348,c,av);}
a=C_alloc(5);
t4=t1;{
C_word *av2=av;
av2[0]=t4;
av2[1]=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1350,a[2]=t2,a[3]=t3,a[4]=((C_word)li17),tmp=(C_word)a,a+=5,tmp);
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}

/* f_1350 in O in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1350(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,2)))){
C_save_and_reclaim((void *)f_1350,c,av);}
a=C_alloc(6);
t4=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1358,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=((C_word*)t0)[3],a[5]=t3,tmp=(C_word)a,a+=6,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:25: f"));
t5=((C_word*)t0)[3];{
C_word *av2=av;
av2[0]=t5;
av2[1]=t4;
av2[2]=t2;
((C_proc)C_fast_retrieve_proc(t5))(3,av2);}}

/* k1356 */
static void C_ccall f_1358(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,2)))){
C_save_and_reclaim((void *)f_1358,c,av);}
a=C_alloc(5);
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1362,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t1,tmp=(C_word)a,a+=5,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:25: f"));
t3=((C_word*)t0)[4];{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t3;
av2[1]=t2;
av2[2]=((C_word*)t0)[5];
((C_proc)C_fast_retrieve_proc(t3))(3,av2);}}

/* k1360 in k1356 */
static void C_ccall f_1362(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1362,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:25: g"));
t2=((C_word*)t0)[2];{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[3];
av2[2]=((C_word*)t0)[4];
av2[3]=t1;
((C_proc)C_fast_retrieve_proc(t2))(4,av2);}}

/* &&& in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1364(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand((c-2)*C_SIZEOF_PAIR +4,c,3)))){
C_save_and_reclaim((void*)f_1364,c,av);}
a=C_alloc((c-2)*C_SIZEOF_PAIR+4);
t2=C_build_rest(&a,c,2,av);
C_word t3;
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1366,a[2]=t2,a[3]=((C_word)li21),tmp=(C_word)a,a+=4,tmp);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* f_1366 in &&& in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1366(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,2)))){
C_save_and_reclaim((void *)f_1366,c,av);}
a=C_alloc(5);
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1381,a[2]=t1,a[3]=((C_word*)t0)[2],a[4]=t2,tmp=(C_word)a,a+=5,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:26: ⍨"));
t4=C_fast_retrieve(lf[38]);{
C_word *av2=av;
av2[0]=t4;
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[31]);
((C_proc)(void*)(*((C_word*)t4+1)))(3,av2);}}

/* k1379 */
static void C_ccall f_1381(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(8,c,3)))){
C_save_and_reclaim((void *)f_1381,c,av);}
a=C_alloc(8);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f2233,a[2]=t1,a[3]=((C_word)li20),tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f2246,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:26: g350"));
t4=t2;
f2233(t4,t3,((C_word*)t0)[4]);}

/* *** in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1383(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand((c-2)*C_SIZEOF_PAIR +4,c,3)))){
C_save_and_reclaim((void*)f_1383,c,av);}
a=C_alloc((c-2)*C_SIZEOF_PAIR+4);
t2=C_build_rest(&a,c,2,av);
C_word t3;
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1385,a[2]=t2,a[3]=((C_word)li23),tmp=(C_word)a,a+=4,tmp);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* f_1385 in *** in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1385(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,4)))){
C_save_and_reclaim((void *)f_1385,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:27: ∀"));
t3=*((C_word*)lf[26]+1);{
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=t3;
av2[1]=t1;
av2[2]=C_fast_retrieve(lf[31]);
av2[3]=((C_word*)t0)[2];
av2[4]=t2;
((C_proc)(void*)(*((C_word*)t3+1)))(5,av2);}}

/* k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1393(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word t13;
C_word t14;
C_word t15;
C_word t16;
C_word t17;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(39,c,4)))){
C_save_and_reclaim((void *)f_1393,c,av);}
a=C_alloc(39);
t2=C_mutate((C_word*)lf[61]+1 /* (set! & ...) */,t1);
t3=C_mutate((C_word*)lf[62]+1 /* (set! tap ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1395,a[2]=((C_word)li25),tmp=(C_word)a,a+=3,tmp));
t4=C_mutate((C_word*)lf[63]+1 /* (set! ∧∧ ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1401,a[2]=((C_word)li26),tmp=(C_word)a,a+=3,tmp));
t5=C_mutate((C_word*)lf[64]+1 /* (set! ∨∨ ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1407,a[2]=((C_word)li27),tmp=(C_word)a,a+=3,tmp));
t6=C_mutate((C_word*)lf[65]+1 /* (set! ∈ ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1413,a[2]=((C_word)li28),tmp=(C_word)a,a+=3,tmp));
t7=C_mutate((C_word*)lf[71]+1 /* (set! conj ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1430,a[2]=((C_word)li29),tmp=(C_word)a,a+=3,tmp));
t8=C_mutate((C_word*)lf[72]+1 /* (set! ⊃ ...) */,C_fast_retrieve(lf[71]));
t9=C_mutate((C_word*)lf[73]+1 /* (set! ↑n ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1441,a[2]=((C_word)li30),tmp=(C_word)a,a+=3,tmp));
t10=C_mutate((C_word*)lf[75]+1 /* (set! ↓n ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1459,a[2]=((C_word)li31),tmp=(C_word)a,a+=3,tmp));
t11=C_mutate((C_word*)lf[77]+1 /* (set! ⍋ ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1477,a[2]=((C_word)li32),tmp=(C_word)a,a+=3,tmp));
t12=C_mutate((C_word*)lf[66]+1 /* (set! right ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1483,a[2]=((C_word)li33),tmp=(C_word)a,a+=3,tmp));
t13=C_mutate((C_word*)lf[79]+1 /* (set! right? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1489,a[2]=((C_word)li34),tmp=(C_word)a,a+=3,tmp));
t14=C_mutate((C_word*)lf[80]+1 /* (set! right-value ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1495,a[2]=((C_word)li35),tmp=(C_word)a,a+=3,tmp));
t15=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1505,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t16=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2119,a[2]=((C_word)li94),tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:43: chicken.base#set-record-printer!"));
t17=C_fast_retrieve(lf[149]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t17;
av2[1]=t15;
av2[2]=lf[66];
av2[3]=t16;
((C_proc)(void*)(*((C_word*)t17+1)))(4,av2);}}

/* tap in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1395(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,2)))){
C_save_and_reclaim((void *)f_1395,c,av);}
a=C_alloc(4);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1399,a[2]=t1,a[3]=t3,tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:29: f"));
t5=t2;{
C_word *av2=av;
av2[0]=t5;
av2[1]=t4;
av2[2]=t3;
((C_proc)C_fast_retrieve_proc(t5))(3,av2);}}

/* k1397 in tap in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1399(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_1399,c,av);}
t2=((C_word*)t0)[2];{
C_word *av2=av;
av2[0]=t2;
av2[1]=((C_word*)t0)[3];
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* ~~~~~~ in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1401(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_1401,c,av);}
t4=t1;{
C_word *av2=av;
av2[0]=t4;
av2[1]=(C_truep(t2)?t3:C_SCHEME_FALSE);
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}

/* ~~~~~~ in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1407(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_1407,c,av);}
t4=t1;{
C_word *av2=av;
av2[0]=t4;
av2[1]=(C_truep(t2)?t2:t3);
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}

/* ~~~ in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1413(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_1413,c,av);}
a=C_alloc(4);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1417,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:32: srfi-1#assoc"));
t5=C_fast_retrieve(lf[70]);{
C_word *av2=av;
av2[0]=t5;
av2[1]=t4;
av2[2]=t2;
av2[3]=t3;
((C_proc)(void*)(*((C_word*)t5+1)))(4,av2);}}

/* k1415 in ~~~ in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1417(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_1417,c,av);}
a=C_alloc(4);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1428,a[2]=t1,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:32: ◇"));
t3=C_fast_retrieve(lf[21]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t3;
av2[1]=t2;
av2[2]=lf[69];
av2[3]=((C_word*)t0)[3];
((C_proc)(void*)(*((C_word*)t3+1)))(4,av2);}}

/* k1426 in k1415 in ~~~ in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1428(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,3)))){
C_save_and_reclaim((void *)f_1428,c,av);}
a=C_alloc(3);
t2=(C_truep(((C_word*)t0)[2])?C_a_i_record2(&a,2,lf[66],((C_word*)t0)[2]):C_a_i_record2(&a,2,lf[67],t1));
C_trace(C_text("analysis/helpers/prelude.scm:32: ⊙"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[68]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[68]+1);
av2[1]=((C_word*)t0)[3];
av2[2]=C_fast_retrieve(lf[4]);
av2[3]=t2;
tp(4,av2);}}

/* conj in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1430(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,3)))){
C_save_and_reclaim((void *)f_1430,c,av);}
a=C_alloc(3);
t4=C_a_i_list(&a,1,t3);
C_trace(C_text("analysis/helpers/prelude.scm:33: ##sys#append"));
t5=*((C_word*)lf[52]+1);{
C_word *av2=av;
av2[0]=t5;
av2[1]=t1;
av2[2]=t2;
av2[3]=t4;
((C_proc)(void*)(*((C_word*)t5+1)))(4,av2);}}

/* ~~~n in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1441(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1441,c,av);}
t4=C_i_length(t3);
if(C_truep(C_i_greaterp(t2,t4))){
C_trace(C_text("analysis/helpers/prelude.scm:34: srfi-1#take"));
t5=C_fast_retrieve(lf[74]);{
C_word *av2=av;
av2[0]=t5;
av2[1]=t1;
av2[2]=t3;
av2[3]=t4;
((C_proc)(void*)(*((C_word*)t5+1)))(4,av2);}}
else{
C_trace(C_text("analysis/helpers/prelude.scm:34: srfi-1#take"));
t5=C_fast_retrieve(lf[74]);{
C_word *av2=av;
av2[0]=t5;
av2[1]=t1;
av2[2]=t3;
av2[3]=t2;
((C_proc)(void*)(*((C_word*)t5+1)))(4,av2);}}}

/* ~~~n in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1459(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1459,c,av);}
t4=C_i_length(t3);
if(C_truep(C_i_greaterp(t2,t4))){
C_trace(C_text("analysis/helpers/prelude.scm:35: srfi-1#drop"));
t5=C_fast_retrieve(lf[76]);{
C_word *av2=av;
av2[0]=t5;
av2[1]=t1;
av2[2]=t3;
av2[3]=t4;
((C_proc)(void*)(*((C_word*)t5+1)))(4,av2);}}
else{
C_trace(C_text("analysis/helpers/prelude.scm:35: srfi-1#drop"));
t5=C_fast_retrieve(lf[76]);{
C_word *av2=av;
av2[0]=t5;
av2[1]=t1;
av2[2]=t3;
av2[3]=t2;
((C_proc)(void*)(*((C_word*)t5+1)))(4,av2);}}}

/* ~~~ in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1477(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1477,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:36: chicken.sort#sort"));
t4=C_fast_retrieve(lf[78]);{
C_word *av2=av;
av2[0]=t4;
av2[1]=t1;
av2[2]=t3;
av2[3]=t2;
((C_proc)(void*)(*((C_word*)t4+1)))(4,av2);}}

/* right in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1483(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_1483,c,av);}
a=C_alloc(3);
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=C_a_i_record2(&a,2,lf[66],t2);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* right? in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1489(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_1489,c,av);}
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=C_i_structurep(t2,lf[66]);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* right-value in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1495(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_1495,c,av);}
t3=C_i_check_structure_2(t2,lf[66],lf[80]);
t4=t1;{
C_word *av2=av;
av2[0]=t4;
av2[1]=C_i_block_ref(t2,C_fix(1));
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}

/* k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1505(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,3)))){
C_save_and_reclaim((void *)f_1505,c,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1508,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2105,a[2]=((C_word)li93),tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:46: chicken.read-syntax#set-sharp-read-syntax!"));
t4=C_fast_retrieve(lf[144]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t4;
av2[1]=t2;
av2[2]=C_make_character(82);
av2[3]=t3;
((C_proc)(void*)(*((C_word*)t4+1)))(4,av2);}}

/* k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1508(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(15,c,4)))){
C_save_and_reclaim((void *)f_1508,c,av);}
a=C_alloc(15);
t2=C_mutate((C_word*)lf[67]+1 /* (set! left ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1510,a[2]=((C_word)li36),tmp=(C_word)a,a+=3,tmp));
t3=C_mutate((C_word*)lf[81]+1 /* (set! left? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1516,a[2]=((C_word)li37),tmp=(C_word)a,a+=3,tmp));
t4=C_mutate((C_word*)lf[82]+1 /* (set! left-value ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1522,a[2]=((C_word)li38),tmp=(C_word)a,a+=3,tmp));
t5=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1532,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t6=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2086,a[2]=((C_word)li92),tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:53: chicken.base#set-record-printer!"));
t7=C_fast_retrieve(lf[149]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t7;
av2[1]=t5;
av2[2]=lf[67];
av2[3]=t6;
((C_proc)(void*)(*((C_word*)t7+1)))(4,av2);}}

/* left in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1510(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_1510,c,av);}
a=C_alloc(3);
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=C_a_i_record2(&a,2,lf[67],t2);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* left? in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1516(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_1516,c,av);}
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=C_i_structurep(t2,lf[67]);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* left-value in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1522(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_1522,c,av);}
t3=C_i_check_structure_2(t2,lf[67],lf[82]);
t4=t1;{
C_word *av2=av;
av2[0]=t4;
av2[1]=C_i_block_ref(t2,C_fix(1));
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}

/* k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1532(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,3)))){
C_save_and_reclaim((void *)f_1532,c,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1535,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2072,a[2]=((C_word)li91),tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:56: chicken.read-syntax#set-sharp-read-syntax!"));
t4=C_fast_retrieve(lf[144]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t4;
av2[1]=t2;
av2[2]=C_make_character(76);
av2[3]=t3;
((C_proc)(void*)(*((C_word*)t4+1)))(4,av2);}}

/* k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1535(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(12,c,4)))){
C_save_and_reclaim((void *)f_1535,c,av);}
a=C_alloc(12);
t2=C_mutate((C_word*)lf[83]+1 /* (set! _+ ...) */,C_fast_retrieve(lf[80]));
t3=C_mutate((C_word*)lf[84]+1 /* (set! +_ ...) */,C_fast_retrieve(lf[82]));
t4=C_mutate((C_word*)lf[85]+1 /* (set! _+? ...) */,C_fast_retrieve(lf[79]));
t5=C_mutate((C_word*)lf[86]+1 /* (set! +_? ...) */,C_fast_retrieve(lf[81]));
t6=C_mutate((C_word*)lf[87]+1 /* (set! either? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1541,a[2]=((C_word)li39),tmp=(C_word)a,a+=3,tmp));
t7=C_mutate((C_word*)lf[88]+1 /* (set! either-guard ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1553,a[2]=((C_word)li40),tmp=(C_word)a,a+=3,tmp));
t8=C_mutate((C_word*)lf[91]+1 /* (set! gett ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1568,a[2]=((C_word)li41),tmp=(C_word)a,a+=3,tmp));
t9=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1589,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:77: D"));
t10=C_fast_retrieve(lf[51]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t10;
av2[1]=t9;
av2[2]=C_fast_retrieve(lf[88]);
av2[3]=C_fast_retrieve(lf[91]);
f_1276(4,av2);}}

/* either? in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1541(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,2)))){
C_save_and_reclaim((void *)f_1541,c,av);}
a=C_alloc(4);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1545,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:74: +_?"));
t4=C_fast_retrieve(lf[86]);{
C_word *av2=av;
av2[0]=t4;
av2[1]=t3;
av2[2]=t2;
((C_proc)(void*)(*((C_word*)t4+1)))(3,av2);}}

/* k1543 in either? in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1545(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_1545,c,av);}
if(C_truep(t1)){
t2=((C_word*)t0)[2];{
C_word *av2=av;
av2[0]=t2;
av2[1]=t1;
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}
else{
C_trace(C_text("analysis/helpers/prelude.scm:74: _+?"));
t2=C_fast_retrieve(lf[85]);{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
((C_proc)(void*)(*((C_word*)t2+1)))(3,av2);}}}

/* either-guard in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1553(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,2)))){
C_save_and_reclaim((void *)f_1553,c,av);}
a=C_alloc(5);
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1560,a[2]=t2,a[3]=t1,a[4]=t3,tmp=(C_word)a,a+=5,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:75: either?"));
t5=C_fast_retrieve(lf[87]);{
C_word *av2=av;
av2[0]=t5;
av2[1]=t4;
av2[2]=t3;
f_1541(3,av2);}}

/* k1558 in either-guard in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1560(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1560,c,av);}
if(C_truep(t1)){
C_trace(C_text("analysis/helpers/prelude.scm:75: f"));
t2=((C_word*)t0)[2];{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[3];
av2[2]=((C_word*)t0)[4];
((C_proc)C_fast_retrieve_proc(t2))(3,av2);}}
else{
C_trace(C_text("analysis/helpers/prelude.scm:75: chicken.base#error"));
t2=*((C_word*)lf[89]+1);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[3];
av2[2]=lf[90];
av2[3]=((C_word*)t0)[4];
((C_proc)(void*)(*((C_word*)t2+1)))(4,av2);}}}

/* gett in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1568(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,2)))){
C_save_and_reclaim((void *)f_1568,c,av);}
a=C_alloc(4);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1575,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:76: _+?"));
t4=C_fast_retrieve(lf[85]);{
C_word *av2=av;
av2[0]=t4;
av2[1]=t3;
av2[2]=t2;
((C_proc)(void*)(*((C_word*)t4+1)))(3,av2);}}

/* k1573 in gett in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1575(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_1575,c,av);}
a=C_alloc(3);
if(C_truep(t1)){
C_trace(C_text("analysis/helpers/prelude.scm:76: _+"));
t2=C_fast_retrieve(lf[83]);{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
((C_proc)(void*)(*((C_word*)t2+1)))(3,av2);}}
else{
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1585,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:76: +_"));
t3=C_fast_retrieve(lf[84]);{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t3;
av2[1]=t2;
av2[2]=((C_word*)t0)[3];
((C_proc)(void*)(*((C_word*)t3+1)))(3,av2);}}}

/* k1583 in k1573 in gett in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1585(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_1585,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:76: chicken.base#error"));
t2=*((C_word*)lf[89]+1);{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
((C_proc)(void*)(*((C_word*)t2+1)))(3,av2);}}

/* k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1589(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word t7;
C_word t8;
C_word t9;
C_word t10;
C_word t11;
C_word t12;
C_word t13;
C_word t14;
C_word t15;
C_word t16;
C_word t17;
C_word t18;
C_word t19;
C_word t20;
C_word t21;
C_word t22;
C_word t23;
C_word t24;
C_word t25;
C_word t26;
C_word t27;
C_word t28;
C_word t29;
C_word t30;
C_word t31;
C_word t32;
C_word t33;
C_word t34;
C_word t35;
C_word t36;
C_word t37;
C_word t38;
C_word t39;
C_word t40;
C_word t41;
C_word t42;
C_word t43;
C_word t44;
C_word t45;
C_word t46;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(93,c,5)))){
C_save_and_reclaim((void *)f_1589,c,av);}
a=C_alloc(93);
t2=C_mutate((C_word*)lf[92]+1 /* (set! get ...) */,t1);
t3=C_mutate((C_word*)lf[93]+1 /* (set! ensure ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1591,a[2]=((C_word)li42),tmp=(C_word)a,a+=3,tmp));
t4=C_mutate((C_word*)lf[94]+1 /* (set! fmapp ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1603,a[2]=((C_word)li43),tmp=(C_word)a,a+=3,tmp));
t5=C_mutate((C_word*)lf[95]+1 /* (set! fmap ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1623,a[2]=((C_word)li44),tmp=(C_word)a,a+=3,tmp));
t6=C_mutate((C_word*)lf[96]+1 /* (set! bindd ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1633,a[2]=((C_word)li45),tmp=(C_word)a,a+=3,tmp));
t7=C_mutate((C_word*)lf[97]+1 /* (set! bind ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1649,a[2]=((C_word)li46),tmp=(C_word)a,a+=3,tmp));
t8=C_mutate((C_word*)lf[98]+1 /* (set! fjoinn ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1659,a[2]=((C_word)li47),tmp=(C_word)a,a+=3,tmp));
t9=C_mutate((C_word*)lf[99]+1 /* (set! fjoin ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1665,a[2]=((C_word)li48),tmp=(C_word)a,a+=3,tmp));
t10=C_mutate((C_word*)lf[68]+1 /* (set! ⊙ ...) */,C_fast_retrieve(lf[95]));
t11=C_mutate((C_word*)lf[100]+1 /* (set! ⊥ ...) */,C_fast_retrieve(lf[99]));
t12=C_mutate((C_word*)lf[101]+1 /* (set! >>= ...) */,C_fast_retrieve(lf[97]));
t13=C_mutate((C_word*)lf[102]+1 /* (set! thenn ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1674,a[2]=((C_word)li50),tmp=(C_word)a,a+=3,tmp));
t14=C_mutate((C_word*)lf[103]+1 /* (set! then ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1684,a[2]=((C_word)li51),tmp=(C_word)a,a+=3,tmp));
t15=C_mutate((C_word*)lf[104]+1 /* (set! ass ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1694,a[2]=((C_word)li53),tmp=(C_word)a,a+=3,tmp));
t16=C_mutate((C_word*)lf[105]+1 /* (set! as ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1704,a[2]=((C_word)li55),tmp=(C_word)a,a+=3,tmp));
t17=C_mutate((C_word*)lf[106]+1 /* (set! *> ...) */,C_fast_retrieve(lf[103]));
t18=C_mutate((C_word*)lf[107]+1 /* (set! $> ...) */,C_fast_retrieve(lf[105]));
t19=C_mutate((C_word*)lf[108]+1 /* (set! lift22 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1716,a[2]=((C_word)li58),tmp=(C_word)a,a+=3,tmp));
t20=C_mutate((C_word*)lf[109]+1 /* (set! lift2 ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1734,a[2]=((C_word)li61),tmp=(C_word)a,a+=3,tmp));
t21=C_mutate((C_word*)lf[110]+1 /* (set! fcomposee ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1752,a[2]=((C_word)li62),tmp=(C_word)a,a+=3,tmp));
t22=C_mutate((C_word*)lf[111]+1 /* (set! fcompose ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1758,a[2]=((C_word)li63),tmp=(C_word)a,a+=3,tmp));
t23=C_mutate((C_word*)lf[112]+1 /* (set! kleislii ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1764,a[2]=((C_word)li64),tmp=(C_word)a,a+=3,tmp));
t24=C_mutate((C_word*)lf[113]+1 /* (set! kleisli ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1770,a[2]=((C_word)li65),tmp=(C_word)a,a+=3,tmp));
t25=C_mutate((C_word*)lf[114]+1 /* (set! ◁ ...) */,C_fast_retrieve(lf[111]));
t26=C_mutate((C_word*)lf[115]+1 /* (set! ◀ ...) */,C_fast_retrieve(lf[113]));
t27=C_mutate((C_word*)lf[116]+1 /* (set! break-left ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1778,a[2]=((C_word)li66),tmp=(C_word)a,a+=3,tmp));
t28=C_mutate((C_word*)lf[117]+1 /* (set! △+_ ...) */,C_fast_retrieve(lf[116]));
t29=C_mutate((C_word*)lf[118]+1 /* (set! sequencee ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1791,a[2]=((C_word)li69),tmp=(C_word)a,a+=3,tmp));
t30=C_mutate((C_word*)lf[120]+1 /* (set! sequence ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1817,a[2]=((C_word)li72),tmp=(C_word)a,a+=3,tmp));
t31=C_mutate((C_word*)lf[121]+1 /* (set! traversee ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1843,a[2]=((C_word)li73),tmp=(C_word)a,a+=3,tmp));
t32=C_mutate((C_word*)lf[122]+1 /* (set! traverse ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1853,a[2]=((C_word)li74),tmp=(C_word)a,a+=3,tmp));
t33=C_mutate((C_word*)lf[123]+1 /* (set! lmapp ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1863,a[2]=((C_word)li75),tmp=(C_word)a,a+=3,tmp));
t34=C_mutate((C_word*)lf[124]+1 /* (set! lmap ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1883,a[2]=((C_word)li76),tmp=(C_word)a,a+=3,tmp));
t35=C_mutate((C_word*)lf[125]+1 /* (set! ⊙_ ...) */,C_fast_retrieve(lf[124]));
t36=C_mutate((C_word*)lf[126]+1 /* (set! split-choicee ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1894,a[2]=((C_word)li77),tmp=(C_word)a,a+=3,tmp));
t37=C_mutate((C_word*)lf[127]+1 /* (set! split-choice ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1904,a[2]=((C_word)li78),tmp=(C_word)a,a+=3,tmp));
t38=C_mutate((C_word*)lf[128]+1 /* (set! fan-inn ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1914,a[2]=((C_word)li79),tmp=(C_word)a,a+=3,tmp));
t39=C_mutate((C_word*)lf[129]+1 /* (set! fan-in ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1937,a[2]=((C_word)li80),tmp=(C_word)a,a+=3,tmp));
t40=C_mutate((C_word*)lf[130]+1 /* (set! +++ ...) */,C_fast_retrieve(lf[127]));
t41=C_mutate((C_word*)lf[131]+1 /* (set! /// ...) */,C_fast_retrieve(lf[129]));
t42=C_mutate((C_word*)lf[132]+1 /* (set! alternative ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1949,a[2]=((C_word)li83),tmp=(C_word)a,a+=3,tmp));
t43=C_mutate((C_word*)lf[133]+1 /* (set! </> ...) */,C_fast_retrieve(lf[132]));
t44=C_mutate((C_word*)lf[134]+1 /* (set! ι ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1974,a[2]=((C_word)li90),tmp=(C_word)a,a+=3,tmp));
t45=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2070,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("chicken.base#implicit-exit-handler"));
t46=C_fast_retrieve(lf[142]);{
C_word *av2=av;
av2[0]=t46;
av2[1]=t45;
((C_proc)(void*)(*((C_word*)t46+1)))(2,av2);}}

/* ensure in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1591(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4=av[4];
C_word t5;
C_word *a;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_1591,c,av);}
a=C_alloc(3);
t5=t1;{
C_word *av2=av;
av2[0]=t5;
av2[1]=(C_truep(t2)?C_a_i_record2(&a,2,lf[66],t4):C_a_i_record2(&a,2,lf[67],t3));
((C_proc)(void*)(*((C_word*)t5+1)))(2,av2);}}

/* fmapp in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1603(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,2)))){
C_save_and_reclaim((void *)f_1603,c,av);}
a=C_alloc(5);
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1610,a[2]=t1,a[3]=t2,a[4]=t3,tmp=(C_word)a,a+=5,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:79: _+?"));
t5=C_fast_retrieve(lf[85]);{
C_word *av2=av;
av2[0]=t5;
av2[1]=t4;
av2[2]=t3;
((C_proc)(void*)(*((C_word*)t5+1)))(3,av2);}}

/* k1608 in fmapp in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1610(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,2)))){
C_save_and_reclaim((void *)f_1610,c,av);}
a=C_alloc(7);
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1617,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1621,a[2]=((C_word*)t0)[3],a[3]=t2,tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:79: _+"));
t4=C_fast_retrieve(lf[83]);{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t4;
av2[1]=t3;
av2[2]=((C_word*)t0)[4];
((C_proc)(void*)(*((C_word*)t4+1)))(3,av2);}}
else{
t2=((C_word*)t0)[2];{
C_word *av2=av;
av2[0]=t2;
av2[1]=((C_word*)t0)[4];
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}}

/* k1615 in k1608 in fmapp in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1617(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_1617,c,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av;
av2[0]=t2;
av2[1]=C_a_i_record2(&a,2,lf[66],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* k1619 in k1608 in fmapp in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1621(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_1621,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:79: f"));
t2=((C_word*)t0)[2];{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[3];
av2[2]=t1;
((C_proc)C_fast_retrieve_proc(t2))(3,av2);}}

/* fmap in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1623(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_1623,c,av);}
a=C_alloc(4);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1631,a[2]=t1,a[3]=t3,tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:80: D"));
t5=C_fast_retrieve(lf[51]);{
C_word *av2=av;
av2[0]=t5;
av2[1]=t4;
av2[2]=C_fast_retrieve(lf[94]);
av2[3]=t2;
f_1276(4,av2);}}

/* k1629 in fmap in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1631(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1631,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:80: either-guard"));
t2=C_fast_retrieve(lf[88]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
av2[3]=((C_word*)t0)[3];
f_1553(4,av2);}}

/* bindd in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1633(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,2)))){
C_save_and_reclaim((void *)f_1633,c,av);}
a=C_alloc(5);
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1640,a[2]=t2,a[3]=t1,a[4]=t3,tmp=(C_word)a,a+=5,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:81: _+?"));
t5=C_fast_retrieve(lf[85]);{
C_word *av2=av;
av2[0]=t5;
av2[1]=t4;
av2[2]=t3;
((C_proc)(void*)(*((C_word*)t5+1)))(3,av2);}}

/* k1638 in bindd in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1640(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,2)))){
C_save_and_reclaim((void *)f_1640,c,av);}
a=C_alloc(4);
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1647,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:81: _+"));
t3=C_fast_retrieve(lf[83]);{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t3;
av2[1]=t2;
av2[2]=((C_word*)t0)[4];
((C_proc)(void*)(*((C_word*)t3+1)))(3,av2);}}
else{
t2=((C_word*)t0)[3];{
C_word *av2=av;
av2[0]=t2;
av2[1]=((C_word*)t0)[4];
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}}

/* k1645 in k1638 in bindd in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1647(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_1647,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:81: f"));
t2=((C_word*)t0)[2];{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[3];
av2[2]=t1;
((C_proc)C_fast_retrieve_proc(t2))(3,av2);}}

/* bind in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1649(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_1649,c,av);}
a=C_alloc(4);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1657,a[2]=t1,a[3]=t3,tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:82: D"));
t5=C_fast_retrieve(lf[51]);{
C_word *av2=av;
av2[0]=t5;
av2[1]=t4;
av2[2]=C_fast_retrieve(lf[96]);
av2[3]=t2;
f_1276(4,av2);}}

/* k1655 in bind in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1657(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1657,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:82: either-guard"));
t2=C_fast_retrieve(lf[88]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
av2[3]=((C_word*)t0)[3];
f_1553(4,av2);}}

/* fjoinn in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1659(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1659,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:83: bindd"));
t3=C_fast_retrieve(lf[96]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t3;
av2[1]=t1;
av2[2]=C_fast_retrieve(lf[48]);
av2[3]=t2;
f_1633(4,av2);}}

/* fjoin in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1665(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1665,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:84: either-guard"));
t3=C_fast_retrieve(lf[88]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t3;
av2[1]=t1;
av2[2]=C_fast_retrieve(lf[98]);
av2[3]=t2;
f_1553(4,av2);}}

/* thenn in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1674(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_1674,c,av);}
a=C_alloc(4);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f2176,a[2]=t3,a[3]=((C_word)li49),tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:86: bindd"));
t5=C_fast_retrieve(lf[96]);{
C_word *av2=av;
av2[0]=t5;
av2[1]=t1;
av2[2]=t4;
av2[3]=t2;
f_1633(4,av2);}}

/* then in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1684(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_1684,c,av);}
a=C_alloc(4);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1692,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:87: either-guard"));
t5=C_fast_retrieve(lf[88]);{
C_word *av2=av;
av2[0]=t5;
av2[1]=t4;
av2[2]=C_fast_retrieve(lf[49]);
av2[3]=t3;
f_1553(4,av2);}}

/* k1690 in then in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1692(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1692,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:87: >>="));
t2=C_fast_retrieve(lf[101]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
av2[3]=((C_word*)t0)[3];
((C_proc)(void*)(*((C_word*)t2+1)))(4,av2);}}

/* ass in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1694(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_1694,c,av);}
a=C_alloc(4);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f2181,a[2]=t3,a[3]=((C_word)li52),tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:88: fmapp"));
t5=C_fast_retrieve(lf[94]);{
C_word *av2=av;
av2[0]=t5;
av2[1]=t1;
av2[2]=t4;
av2[3]=t2;
f_1603(4,av2);}}

/* as in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1704(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_1704,c,av);}
a=C_alloc(4);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f2186,a[2]=t3,a[3]=((C_word)li54),tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:89: ⊙"));
t5=C_fast_retrieve(lf[68]);{
C_word *av2=av;
av2[0]=t5;
av2[1]=t1;
av2[2]=t4;
av2[3]=t2;
((C_proc)(void*)(*((C_word*)t5+1)))(4,av2);}}

/* lift22 in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1716(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4=av[4];
C_word t5;
C_word t6;
C_word *a;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,3)))){
C_save_and_reclaim((void *)f_1716,c,av);}
a=C_alloc(5);
t5=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1722,a[2]=t2,a[3]=t4,a[4]=((C_word)li57),tmp=(C_word)a,a+=5,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:91: bindd"));
t6=C_fast_retrieve(lf[96]);{
C_word *av2=av;
av2[0]=t6;
av2[1]=t1;
av2[2]=t5;
av2[3]=t3;
f_1633(4,av2);}}

/* a1721 in lift22 in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1722(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,3)))){
C_save_and_reclaim((void *)f_1722,c,av);}
a=C_alloc(5);
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1728,a[2]=((C_word*)t0)[2],a[3]=t2,a[4]=((C_word)li56),tmp=(C_word)a,a+=5,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:91: fmapp"));
t4=C_fast_retrieve(lf[94]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t4;
av2[1]=t1;
av2[2]=t3;
av2[3]=((C_word*)t0)[3];
f_1603(4,av2);}}

/* a1727 in a1721 in lift22 in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1728(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1728,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:91: f"));
t3=((C_word*)t0)[2];{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t3;
av2[1]=t1;
av2[2]=((C_word*)t0)[3];
av2[3]=t2;
((C_proc)C_fast_retrieve_proc(t3))(4,av2);}}

/* lift2 in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1734(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4=av[4];
C_word t5;
C_word t6;
C_word *a;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,3)))){
C_save_and_reclaim((void *)f_1734,c,av);}
a=C_alloc(5);
t5=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1740,a[2]=t2,a[3]=t4,a[4]=((C_word)li60),tmp=(C_word)a,a+=5,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:92: >>="));
t6=C_fast_retrieve(lf[101]);{
C_word *av2=av;
av2[0]=t6;
av2[1]=t1;
av2[2]=t5;
av2[3]=t3;
((C_proc)(void*)(*((C_word*)t6+1)))(4,av2);}}

/* a1739 in lift2 in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1740(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,3)))){
C_save_and_reclaim((void *)f_1740,c,av);}
a=C_alloc(5);
t3=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1746,a[2]=((C_word*)t0)[2],a[3]=t2,a[4]=((C_word)li59),tmp=(C_word)a,a+=5,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:92: ⊙"));
t4=C_fast_retrieve(lf[68]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t4;
av2[1]=t1;
av2[2]=t3;
av2[3]=((C_word*)t0)[3];
((C_proc)(void*)(*((C_word*)t4+1)))(4,av2);}}

/* a1745 in a1739 in lift2 in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1746(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1746,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:92: f"));
t3=((C_word*)t0)[2];{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t3;
av2[1]=t1;
av2[2]=((C_word*)t0)[3];
av2[3]=t2;
((C_proc)C_fast_retrieve_proc(t3))(4,av2);}}

/* fcomposee in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1752(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1752,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:93: D"));
t3=C_fast_retrieve(lf[51]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t3;
av2[1]=t1;
av2[2]=C_fast_retrieve(lf[94]);
av2[3]=t2;
f_1276(4,av2);}}

/* fcompose in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1758(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1758,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:94: D"));
t3=C_fast_retrieve(lf[51]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t3;
av2[1]=t1;
av2[2]=C_fast_retrieve(lf[68]);
av2[3]=t2;
f_1276(4,av2);}}

/* kleislii in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1764(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1764,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:95: D"));
t3=C_fast_retrieve(lf[51]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t3;
av2[1]=t1;
av2[2]=C_fast_retrieve(lf[96]);
av2[3]=t2;
f_1276(4,av2);}}

/* kleisli in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1770(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1770,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:96: D"));
t3=C_fast_retrieve(lf[51]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t3;
av2[1]=t1;
av2[2]=C_fast_retrieve(lf[101]);
av2[3]=t2;
f_1276(4,av2);}}

/* break-left in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1778(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,2)))){
C_save_and_reclaim((void *)f_1778,c,av);}
a=C_alloc(5);
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1785,a[2]=t2,a[3]=t1,a[4]=t3,tmp=(C_word)a,a+=5,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:98: +_?"));
t5=C_fast_retrieve(lf[86]);{
C_word *av2=av;
av2[0]=t5;
av2[1]=t4;
av2[2]=t3;
((C_proc)(void*)(*((C_word*)t5+1)))(3,av2);}}

/* k1783 in break-left in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1785(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_1785,c,av);}
if(C_truep(t1)){
C_trace(C_text("analysis/helpers/prelude.scm:98: △"));
t2=((C_word*)t0)[2];{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[3];
av2[2]=((C_word*)t0)[4];
((C_proc)C_fast_retrieve_proc(t2))(3,av2);}}
else{
t2=((C_word*)t0)[3];{
C_word *av2=av;
av2[0]=t2;
av2[1]=((C_word*)t0)[4];
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}}

/* sequencee in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1791(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_1791,c,av);}
a=C_alloc(4);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1797,a[2]=t2,a[3]=((C_word)li68),tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:101: chicken.base#call/cc"));
t4=*((C_word*)lf[119]+1);{
C_word *av2=av;
av2[0]=t4;
av2[1]=t1;
av2[2]=t3;
((C_proc)(void*)(*((C_word*)t4+1)))(3,av2);}}

/* a1796 in sequencee in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1797(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,4)))){
C_save_and_reclaim((void *)f_1797,c,av);}
a=C_alloc(7);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1803,a[2]=t2,a[3]=((C_word)li67),tmp=(C_word)a,a+=4,tmp);
t4=C_fast_retrieve(lf[14]);
t5=C_a_i_record2(&a,2,lf[66],C_fast_retrieve(lf[14]));
C_trace(C_text("analysis/helpers/prelude.scm:101: ⇒"));
t6=*((C_word*)lf[33]+1);{
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=t6;
av2[1]=t1;
av2[2]=t3;
av2[3]=t5;
av2[4]=((C_word*)t0)[2];
((C_proc)(void*)(*((C_word*)t6+1)))(5,av2);}}

/* a1802 in a1796 in sequencee in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1803(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,4)))){
C_save_and_reclaim((void *)f_1803,c,av);}
a=C_alloc(4);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1811,a[2]=t1,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:101: lift22"));
t5=C_fast_retrieve(lf[108]);{
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=t5;
av2[1]=t4;
av2[2]=C_fast_retrieve(lf[23]);
av2[3]=t2;
av2[4]=t3;
f_1716(5,av2);}}

/* k1809 in a1802 in a1796 in sequencee in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1811(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1811,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:101: △+_"));
t2=C_fast_retrieve(lf[117]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
((C_proc)(void*)(*((C_word*)t2+1)))(4,av2);}}

/* sequence in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1817(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_1817,c,av);}
a=C_alloc(4);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1823,a[2]=t2,a[3]=((C_word)li71),tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:103: chicken.base#call/cc"));
t4=*((C_word*)lf[119]+1);{
C_word *av2=av;
av2[0]=t4;
av2[1]=t1;
av2[2]=t3;
((C_proc)(void*)(*((C_word*)t4+1)))(3,av2);}}

/* a1822 in sequence in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1823(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,4)))){
C_save_and_reclaim((void *)f_1823,c,av);}
a=C_alloc(7);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1829,a[2]=t2,a[3]=((C_word)li70),tmp=(C_word)a,a+=4,tmp);
t4=C_fast_retrieve(lf[14]);
t5=C_a_i_record2(&a,2,lf[66],C_fast_retrieve(lf[14]));
C_trace(C_text("analysis/helpers/prelude.scm:103: ⇒"));
t6=*((C_word*)lf[33]+1);{
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=t6;
av2[1]=t1;
av2[2]=t3;
av2[3]=t5;
av2[4]=((C_word*)t0)[2];
((C_proc)(void*)(*((C_word*)t6+1)))(5,av2);}}

/* a1828 in a1822 in sequence in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1829(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,4)))){
C_save_and_reclaim((void *)f_1829,c,av);}
a=C_alloc(4);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1837,a[2]=t1,a[3]=((C_word*)t0)[2],tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:103: lift2"));
t5=C_fast_retrieve(lf[109]);{
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=t5;
av2[1]=t4;
av2[2]=C_fast_retrieve(lf[23]);
av2[3]=t2;
av2[4]=t3;
f_1734(5,av2);}}

/* k1835 in a1828 in a1822 in sequence in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1837(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1837,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:103: △+_"));
t2=C_fast_retrieve(lf[117]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
((C_proc)(void*)(*((C_word*)t2+1)))(4,av2);}}

/* traversee in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1843(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,3)))){
C_save_and_reclaim((void *)f_1843,c,av);}
a=C_alloc(3);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1851,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("##sys#map"));
t5=*((C_word*)lf[59]+1);{
C_word *av2=av;
av2[0]=t5;
av2[1]=t4;
av2[2]=t2;
av2[3]=t3;
((C_proc)(void*)(*((C_word*)t5+1)))(4,av2);}}

/* k1849 in traversee in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1851(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_1851,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:104: sequencee"));
t2=C_fast_retrieve(lf[118]);{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
f_1791(3,av2);}}

/* traverse in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1853(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,3)))){
C_save_and_reclaim((void *)f_1853,c,av);}
a=C_alloc(3);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1861,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("##sys#map"));
t5=*((C_word*)lf[59]+1);{
C_word *av2=av;
av2[0]=t5;
av2[1]=t4;
av2[2]=t2;
av2[3]=t3;
((C_proc)(void*)(*((C_word*)t5+1)))(4,av2);}}

/* k1859 in traverse in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1861(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_1861,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:105: sequence"));
t2=C_fast_retrieve(lf[120]);{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
f_1817(3,av2);}}

/* lmapp in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1863(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,2)))){
C_save_and_reclaim((void *)f_1863,c,av);}
a=C_alloc(5);
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1870,a[2]=t1,a[3]=t2,a[4]=t3,tmp=(C_word)a,a+=5,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:106: +_?"));
t5=C_fast_retrieve(lf[86]);{
C_word *av2=av;
av2[0]=t5;
av2[1]=t4;
av2[2]=t3;
((C_proc)(void*)(*((C_word*)t5+1)))(3,av2);}}

/* k1868 in lmapp in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1870(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,2)))){
C_save_and_reclaim((void *)f_1870,c,av);}
a=C_alloc(7);
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1877,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1881,a[2]=((C_word*)t0)[3],a[3]=t2,tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:106: +_"));
t4=C_fast_retrieve(lf[84]);{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t4;
av2[1]=t3;
av2[2]=((C_word*)t0)[4];
((C_proc)(void*)(*((C_word*)t4+1)))(3,av2);}}
else{
t2=((C_word*)t0)[2];{
C_word *av2=av;
av2[0]=t2;
av2[1]=((C_word*)t0)[4];
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}}

/* k1875 in k1868 in lmapp in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1877(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_1877,c,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av;
av2[0]=t2;
av2[1]=C_a_i_record2(&a,2,lf[67],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* k1879 in k1868 in lmapp in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1881(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_1881,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:106: f"));
t2=((C_word*)t0)[2];{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[3];
av2[2]=t1;
((C_proc)C_fast_retrieve_proc(t2))(3,av2);}}

/* lmap in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1883(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_1883,c,av);}
a=C_alloc(4);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1891,a[2]=t1,a[3]=t3,tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:107: D"));
t5=C_fast_retrieve(lf[51]);{
C_word *av2=av;
av2[0]=t5;
av2[1]=t4;
av2[2]=C_fast_retrieve(lf[123]);
av2[3]=t2;
f_1276(4,av2);}}

/* k1889 in lmap in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1891(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1891,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:107: either-guard"));
t2=C_fast_retrieve(lf[88]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
av2[3]=((C_word*)t0)[3];
f_1553(4,av2);}}

/* split-choicee in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1894(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4=av[4];
C_word t5;
C_word t6;
C_word *a;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_1894,c,av);}
a=C_alloc(4);
t5=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1902,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:109: fmapp"));
t6=C_fast_retrieve(lf[94]);{
C_word *av2=av;
av2[0]=t6;
av2[1]=t5;
av2[2]=t3;
av2[3]=t4;
f_1603(4,av2);}}

/* k1900 in split-choicee in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1902(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1902,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:109: lmapp"));
t2=C_fast_retrieve(lf[123]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
f_1863(4,av2);}}

/* split-choice in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1904(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4=av[4];
C_word t5;
C_word t6;
C_word *a;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,3)))){
C_save_and_reclaim((void *)f_1904,c,av);}
a=C_alloc(4);
t5=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1912,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:110: ⊙"));
t6=C_fast_retrieve(lf[68]);{
C_word *av2=av;
av2[0]=t6;
av2[1]=t5;
av2[2]=t3;
av2[3]=t4;
((C_proc)(void*)(*((C_word*)t6+1)))(4,av2);}}

/* k1910 in split-choice in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1912(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1912,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:110: ⊙_"));
t2=C_fast_retrieve(lf[125]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
((C_proc)(void*)(*((C_word*)t2+1)))(4,av2);}}

/* fan-inn in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1914(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4=av[4];
C_word t5;
C_word t6;
C_word *a;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,2)))){
C_save_and_reclaim((void *)f_1914,c,av);}
a=C_alloc(6);
t5=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1921,a[2]=t2,a[3]=t1,a[4]=t4,a[5]=t3,tmp=(C_word)a,a+=6,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:111: +_?"));
t6=C_fast_retrieve(lf[86]);{
C_word *av2=av;
av2[0]=t6;
av2[1]=t5;
av2[2]=t4;
((C_proc)(void*)(*((C_word*)t6+1)))(3,av2);}}

/* k1919 in fan-inn in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1921(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,2)))){
C_save_and_reclaim((void *)f_1921,c,av);}
a=C_alloc(4);
if(C_truep(t1)){
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1928,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:111: +_"));
t3=C_fast_retrieve(lf[84]);{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t3;
av2[1]=t2;
av2[2]=((C_word*)t0)[4];
((C_proc)(void*)(*((C_word*)t3+1)))(3,av2);}}
else{
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1935,a[2]=((C_word*)t0)[5],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:111: _+"));
t3=C_fast_retrieve(lf[83]);{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t3;
av2[1]=t2;
av2[2]=((C_word*)t0)[4];
((C_proc)(void*)(*((C_word*)t3+1)))(3,av2);}}}

/* k1926 in k1919 in fan-inn in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1928(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_1928,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:111: g"));
t2=((C_word*)t0)[2];{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[3];
av2[2]=t1;
((C_proc)C_fast_retrieve_proc(t2))(3,av2);}}

/* k1933 in k1919 in fan-inn in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1935(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_1935,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:111: f"));
t2=((C_word*)t0)[2];{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[3];
av2[2]=t1;
((C_proc)C_fast_retrieve_proc(t2))(3,av2);}}

/* fan-in in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1937(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4=av[4];
C_word t5;
C_word t6;
C_word *a;
if(c!=5) C_bad_argc_2(c,5,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,4)))){
C_save_and_reclaim((void *)f_1937,c,av);}
a=C_alloc(4);
t5=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1945,a[2]=t1,a[3]=t4,tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:112: D"));
t6=C_fast_retrieve(lf[51]);{
C_word *av2=av;
av2[0]=t6;
av2[1]=t5;
av2[2]=C_fast_retrieve(lf[128]);
av2[3]=t2;
av2[4]=t3;
f_1276(5,av2);}}

/* k1943 in fan-in in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1945(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1945,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:112: either-guard"));
t2=C_fast_retrieve(lf[88]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
av2[3]=((C_word*)t0)[3];
f_1553(4,av2);}}

/* alternative in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1949(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand((c-2)*C_SIZEOF_PAIR +4,c,3)))){
C_save_and_reclaim((void*)f_1949,c,av);}
a=C_alloc((c-2)*C_SIZEOF_PAIR+4);
t2=C_build_rest(&a,c,2,av);
C_word t3;
C_word t4;
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1955,a[2]=t2,a[3]=((C_word)li82),tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:114: chicken.base#call/cc"));
t4=*((C_word*)lf[119]+1);{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t4;
av2[1]=t1;
av2[2]=t3;
((C_proc)(void*)(*((C_word*)t4+1)))(3,av2);}}

/* a1954 in alternative in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1955(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,4)))){
C_save_and_reclaim((void *)f_1955,c,av);}
a=C_alloc(4);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1961,a[2]=t2,a[3]=((C_word)li81),tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:114: ⇐"));
t4=*((C_word*)lf[35]+1);{
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=t4;
av2[1]=t1;
av2[2]=t3;
av2[3]=C_fast_retrieve(lf[14]);
av2[4]=((C_word*)t0)[2];
((C_proc)(void*)(*((C_word*)t4+1)))(5,av2);}}

/* a1960 in a1954 in alternative in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1961(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,2)))){
C_save_and_reclaim((void *)f_1961,c,av);}
a=C_alloc(5);
t4=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1968,a[2]=((C_word*)t0)[2],a[3]=t1,a[4]=t3,tmp=(C_word)a,a+=5,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:114: _+?"));
t5=C_fast_retrieve(lf[85]);{
C_word *av2=av;
av2[0]=t5;
av2[1]=t4;
av2[2]=t3;
((C_proc)(void*)(*((C_word*)t5+1)))(3,av2);}}

/* k1966 in a1960 in a1954 in alternative in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1968(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_1968,c,av);}
if(C_truep(t1)){
C_trace(C_text("analysis/helpers/prelude.scm:114: △"));
t2=((C_word*)t0)[2];{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[3];
av2[2]=((C_word*)t0)[4];
((C_proc)C_fast_retrieve_proc(t2))(3,av2);}}
else{
t2=((C_word*)t0)[3];{
C_word *av2=av;
av2[0]=t2;
av2[1]=((C_word*)t0)[4];
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}}

/* ~~ in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1974(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(8,c,3)))){
C_save_and_reclaim((void *)f_1974,c,av);}
a=C_alloc(8);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1978,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
t5=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1992,a[2]=t3,a[3]=t2,a[4]=((C_word)li89),tmp=(C_word)a,a+=5,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:116: scheme#call-with-current-continuation"));
t6=*((C_word*)lf[141]+1);{
C_word *av2=av;
av2[0]=t6;
av2[1]=t4;
av2[2]=t5;
((C_proc)(void*)(*((C_word*)t6+1)))(3,av2);}}

/* k1976 in ~~ in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1978(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_1978,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1981,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:116: g860"));
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=t2;
((C_proc)C_fast_retrieve_proc(t3))(2,av2);}}

/* k1979 in k1976 in ~~ in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1981(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_1981,c,av);}
a=C_alloc(3);
t2=C_i_structurep(t1,lf[67]);
t3=((C_word*)t0)[2];{
C_word *av2=av;
av2[0]=t3;
av2[1]=(C_truep(t2)?t1:C_a_i_record2(&a,2,lf[66],t1));
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* a1991 in ~~ in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1992(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word t5;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(10,c,3)))){
C_save_and_reclaim((void *)f_1992,c,av);}
a=C_alloc(10);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1998,a[2]=t2,a[3]=((C_word)li85),tmp=(C_word)a,a+=4,tmp);
t4=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_2035,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t2,a[5]=((C_word)li88),tmp=(C_word)a,a+=6,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:116: chicken.condition#with-exception-handler"));
t5=C_fast_retrieve(lf[140]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t5;
av2[1]=t1;
av2[2]=t3;
av2[3]=t4;
((C_proc)(void*)(*((C_word*)t5+1)))(4,av2);}}

/* a1997 in a1991 in ~~ in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_1998(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,2)))){
C_save_and_reclaim((void *)f_1998,c,av);}
a=C_alloc(4);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2004,a[2]=t2,a[3]=((C_word)li84),tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:116: k857"));
t4=((C_word*)t0)[2];{
C_word *av2=av;
av2[0]=t4;
av2[1]=t1;
av2[2]=t3;
((C_proc)C_fast_retrieve_proc(t4))(3,av2);}}

/* a2003 in a1997 in a1991 in ~~ in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_2004(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,4)))){
C_save_and_reclaim((void *)f_2004,c,av);}
a=C_alloc(3);
t2=C_i_structurep(((C_word*)t0)[2],lf[135]);
t3=(C_truep(t2)?C_slot(((C_word*)t0)[2],C_fix(1)):C_SCHEME_FALSE);
t4=(C_truep(t3)?C_i_memv(lf[136],t3):C_SCHEME_FALSE);
if(C_truep(t4)){
t5=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2021,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:116: chicken.condition#get-condition-property"));
t6=C_fast_retrieve(lf[137]);{
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=t6;
av2[1]=t5;
av2[2]=((C_word*)t0)[2];
av2[3]=lf[136];
av2[4]=lf[138];
((C_proc)(void*)(*((C_word*)t6+1)))(5,av2);}}
else{
C_trace(C_text("analysis/helpers/prelude.scm:116: chicken.condition#signal"));
t5=C_fast_retrieve(lf[139]);{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t5;
av2[1]=t1;
av2[2]=((C_word*)t0)[2];
((C_proc)(void*)(*((C_word*)t5+1)))(3,av2);}}}

/* k2019 in a2003 in a1997 in a1991 in ~~ in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_2021(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,1)))){
C_save_and_reclaim((void *)f_2021,c,av);}
a=C_alloc(3);
t2=((C_word*)t0)[2];{
C_word *av2=av;
av2[0]=t2;
av2[1]=C_a_i_record2(&a,2,lf[67],t1);
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* a2034 in a1991 in ~~ in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_2035(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(14,c,3)))){
C_save_and_reclaim((void *)f_2035,c,av);}
a=C_alloc(14);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2039,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2037,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2047,a[2]=((C_word*)t0)[4],a[3]=((C_word)li87),tmp=(C_word)a,a+=4,tmp);
t5=(
C_trace("tmp11205"),
  f_2037(t3)
);
C_trace(C_text("tmp21206"));
t6=t4;
f_2047(t6,t1,C_a_i_list(&a,1,t5));}

/* tmp11205 in a2034 in a1991 in ~~ in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static C_word C_fcall f_2037(C_word t0){
C_word tmp;
C_word t1;
C_stack_overflow_check;{}
return((
C_trace("analysis/helpers/prelude.scm:116: g872"),
  f_2039(((C_word*)t0)[2])
));}

/* g872 in a2034 in a1991 in ~~ in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static C_word C_fcall f_2039(C_word t0){
C_word tmp;
C_word t1;
C_stack_overflow_check;{}
return(C_i_list_ref(((C_word*)t0)[2],((C_word*)t0)[3]));}

/* tmp21206 in a2034 in a1991 in ~~ in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_fcall f_2047(C_word t0,C_word t1,C_word t2){
C_word tmp;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,0,2)))){
C_save_and_reclaim_args((void *)trf_2047,3,t0,t1,t2);}
a=C_alloc(4);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2053,a[2]=t2,a[3]=((C_word)li86),tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:116: k857"));
t4=((C_word*)t0)[2];{
C_word av2[3];
av2[0]=t4;
av2[1]=t1;
av2[2]=t3;
((C_proc)C_fast_retrieve_proc(t4))(3,av2);}}

/* a2052 in tmp21206 in a2034 in a1991 in ~~ in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_2053(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
if(c!=2) C_bad_argc_2(c,2,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_2053,c,av);}{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=0;
av2[1]=t1;
av2[2]=((C_word*)t0)[2];
C_apply_values(3,av2);}}

/* k2068 in k1587 in k1533 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_2070(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_2070,c,av);}
t2=t1;{
C_word *av2=av;
av2[0]=t2;
av2[1]=((C_word*)t0)[2];
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* a2071 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_2072(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_2072,c,av);}
a=C_alloc(3);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2084,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:56: scheme#read"));
t4=*((C_word*)lf[143]+1);{
C_word *av2=av;
av2[0]=t4;
av2[1]=t3;
av2[2]=t2;
((C_proc)(void*)(*((C_word*)t4+1)))(3,av2);}}

/* k2082 in a2071 in k1530 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_2084(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,1)))){
C_save_and_reclaim((void *)f_2084,c,av);}
a=C_alloc(6);
t2=C_i_car(t1);
t3=((C_word*)t0)[2];{
C_word *av2=av;
av2[0]=t3;
av2[1]=C_a_i_list(&a,2,lf[67],t2);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* a2085 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_2086(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,4)))){
C_save_and_reclaim((void *)f_2086,c,av);}
a=C_alloc(5);
t4=C_i_check_port_2(t3,C_fix(2),C_SCHEME_TRUE,lf[145]);
t5=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2093,a[2]=t1,a[3]=t3,a[4]=t2,tmp=(C_word)a,a+=5,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:54: ##sys#print"));
t6=*((C_word*)lf[147]+1);{
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=t6;
av2[1]=t5;
av2[2]=lf[148];
av2[3]=C_SCHEME_FALSE;
av2[4]=t3;
((C_proc)(void*)(*((C_word*)t6+1)))(5,av2);}}

/* k2091 in a2085 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_2093(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,4)))){
C_save_and_reclaim((void *)f_2093,c,av);}
a=C_alloc(4);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2096,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t3=C_i_check_structure_2(((C_word*)t0)[4],lf[67],lf[82]);
C_trace(C_text("analysis/helpers/prelude.scm:54: ##sys#print"));
t4=*((C_word*)lf[147]+1);{
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=t4;
av2[1]=t2;
av2[2]=C_i_block_ref(((C_word*)t0)[4],C_fix(1));
av2[3]=C_SCHEME_TRUE;
av2[4]=((C_word*)t0)[3];
((C_proc)(void*)(*((C_word*)t4+1)))(5,av2);}}

/* k2094 in k2091 in a2085 in k1506 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_2096(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_2096,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:54: ##sys#write-char-0"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[146]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[146]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=C_make_character(41);
av2[3]=((C_word*)t0)[3];
tp(4,av2);}}

/* a2104 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_2105(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word t4;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_2105,c,av);}
a=C_alloc(3);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_2117,a[2]=t1,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:46: scheme#read"));
t4=*((C_word*)lf[143]+1);{
C_word *av2=av;
av2[0]=t4;
av2[1]=t3;
av2[2]=t2;
((C_proc)(void*)(*((C_word*)t4+1)))(3,av2);}}

/* k2115 in a2104 in k1503 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_2117(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,1)))){
C_save_and_reclaim((void *)f_2117,c,av);}
a=C_alloc(6);
t2=C_i_car(t1);
t3=((C_word*)t0)[2];{
C_word *av2=av;
av2[0]=t3;
av2[1]=C_a_i_list(&a,2,lf[66],t2);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* a2118 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_2119(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3=av[3];
C_word t4;
C_word t5;
C_word t6;
C_word *a;
if(c!=4) C_bad_argc_2(c,4,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,4)))){
C_save_and_reclaim((void *)f_2119,c,av);}
a=C_alloc(5);
t4=C_i_check_port_2(t3,C_fix(2),C_SCHEME_TRUE,lf[145]);
t5=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_2126,a[2]=t1,a[3]=t3,a[4]=t2,tmp=(C_word)a,a+=5,tmp);
C_trace(C_text("analysis/helpers/prelude.scm:44: ##sys#print"));
t6=*((C_word*)lf[147]+1);{
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=t6;
av2[1]=t5;
av2[2]=lf[150];
av2[3]=C_SCHEME_FALSE;
av2[4]=t3;
((C_proc)(void*)(*((C_word*)t6+1)))(5,av2);}}

/* k2124 in a2118 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_2126(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,4)))){
C_save_and_reclaim((void *)f_2126,c,av);}
a=C_alloc(4);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_2129,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t3=C_i_check_structure_2(((C_word*)t0)[4],lf[66],lf[80]);
C_trace(C_text("analysis/helpers/prelude.scm:44: ##sys#print"));
t4=*((C_word*)lf[147]+1);{
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=t4;
av2[1]=t2;
av2[2]=C_i_block_ref(((C_word*)t0)[4],C_fix(1));
av2[3]=C_SCHEME_TRUE;
av2[4]=((C_word*)t0)[3];
((C_proc)(void*)(*((C_word*)t4+1)))(5,av2);}}

/* k2127 in k2124 in a2118 in k1391 in k1224 in k1221 in k1218 in k1215 in k1212 in k1209 */
static void C_ccall f_2129(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_2129,c,av);}
C_trace(C_text("analysis/helpers/prelude.scm:44: ##sys#write-char-0"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[146]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[146]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=C_make_character(41);
av2[3]=((C_word*)t0)[3];
tp(4,av2);}}

/* toplevel */
static C_TLS int toplevel_initialized=0;
C_main_entry_point

void C_ccall C_toplevel(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
if(toplevel_initialized) {C_kontinue(t1,C_SCHEME_UNDEFINED);}
else C_toplevel_entry(C_text("toplevel"));
C_check_nursery_minimum(C_calculate_demand(3,c,2));
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void*)C_toplevel,c,av);}
toplevel_initialized=1;
if(C_unlikely(!C_demand_2(1040))){
C_save(t1);
C_rereclaim2(1040*sizeof(C_word),1);
t1=C_restore;}
a=C_alloc(3);
C_initialize_lf(lf,153);
lf[0]=C_h_intern(&lf[0],3, C_text("\342\206\221"));
lf[1]=C_h_intern(&lf[1],10, C_text("scheme#car"));
lf[2]=C_h_intern(&lf[2],3, C_text("\342\206\223"));
lf[3]=C_h_intern(&lf[3],10, C_text("scheme#cdr"));
lf[4]=C_h_intern(&lf[4],6, C_text("\342\206\221\342\206\223"));
lf[5]=C_h_intern(&lf[5],11, C_text("scheme#cadr"));
lf[6]=C_h_intern(&lf[6],6, C_text("\342\206\221\342\206\221"));
lf[7]=C_h_intern(&lf[7],11, C_text("scheme#caar"));
lf[8]=C_h_intern(&lf[8],6, C_text("\342\206\223\342\206\223"));
lf[9]=C_h_intern(&lf[9],11, C_text("scheme#cddr"));
lf[10]=C_h_intern(&lf[10],3, C_text("\342\210\230"));
lf[11]=C_h_intern(&lf[11],20, C_text("chicken.base#compose"));
lf[12]=C_h_intern(&lf[12],3, C_text("\342\211\241"));
lf[13]=C_h_intern(&lf[13],13, C_text("scheme#equal\077"));
lf[14]=C_h_intern(&lf[14],3, C_text("\342\210\205"));
lf[15]=C_h_intern(&lf[15],4, C_text("\342\210\205\077"));
lf[16]=C_h_intern(&lf[16],12, C_text("scheme#null\077"));
lf[17]=C_h_intern(&lf[17],2, C_text("\317\201"));
lf[18]=C_h_intern(&lf[18],13, C_text("scheme#length"));
lf[19]=C_h_intern(&lf[19],3, C_text("\317\201s"));
lf[20]=C_h_intern(&lf[20],20, C_text("scheme#string-length"));
lf[21]=C_h_intern(&lf[21],3, C_text("\342\227\207"));
lf[22]=C_h_intern(&lf[22],19, C_text("chicken.string#conc"));
lf[23]=C_h_intern(&lf[23],3, C_text("\342\212\202"));
lf[24]=C_h_intern(&lf[24],11, C_text("scheme#cons"));
lf[25]=C_h_intern(&lf[25],3, C_text("\342\210\200"));
lf[26]=C_h_intern(&lf[26],10, C_text("scheme#map"));
lf[27]=C_h_intern(&lf[27],1, C_text("$"));
lf[28]=C_h_intern(&lf[28],12, C_text("scheme#apply"));
lf[29]=C_h_intern(&lf[29],3, C_text("\342\212\226"));
lf[30]=C_h_intern(&lf[30],14, C_text("scheme#reverse"));
lf[31]=C_h_intern(&lf[31],2, C_text("$$"));
lf[32]=C_h_intern(&lf[32],3, C_text("\342\207\222"));
lf[33]=C_h_intern(&lf[33],18, C_text("chicken.base#foldr"));
lf[34]=C_h_intern(&lf[34],3, C_text("\342\207\220"));
lf[35]=C_h_intern(&lf[35],18, C_text("chicken.base#foldl"));
lf[36]=C_h_intern(&lf[36],2, C_text("\302\254"));
lf[37]=C_h_intern(&lf[37],10, C_text("scheme#not"));
lf[38]=C_h_intern(&lf[38],3, C_text("\342\215\250"));
lf[39]=C_h_intern(&lf[39],17, C_text("chicken.base#flip"));
lf[40]=C_h_intern(&lf[40],6, C_text("\342\210\200\342\210\200"));
lf[41]=C_h_intern(&lf[41],15, C_text("scheme#for-each"));
lf[42]=C_h_intern(&lf[42],3, C_text("\342\210\236"));
lf[43]=C_decode_literal(C_heaptop,C_text("\376U+inf.0\000"));
lf[44]=C_h_intern(&lf[44],3, C_text("\342\212\206"));
lf[45]=C_h_intern(&lf[45],11, C_text("scheme#list"));
lf[46]=C_h_intern(&lf[46],4, C_text("\342\210\200\077"));
lf[47]=C_h_intern(&lf[47],13, C_text("srfi-1#filter"));
lf[48]=C_h_intern(&lf[48],1, C_text("I"));
lf[49]=C_h_intern(&lf[49],1, C_text("K"));
lf[50]=C_h_intern(&lf[50],1, C_text("C"));
lf[51]=C_h_intern(&lf[51],1, C_text("D"));
lf[52]=C_h_intern(&lf[52],12, C_text("##sys#append"));
lf[53]=C_h_intern(&lf[53],1, C_text("S"));
lf[54]=C_h_intern(&lf[54],2, C_text("SS"));
lf[55]=C_h_intern(&lf[55],1, C_text("J"));
lf[56]=C_h_intern(&lf[56],2, C_text("JJ"));
lf[57]=C_h_intern(&lf[57],1, C_text("O"));
lf[58]=C_h_intern(&lf[58],3, C_text("&&&"));
lf[59]=C_h_intern(&lf[59],9, C_text("##sys#map"));
lf[60]=C_h_intern(&lf[60],3, C_text("\052\052\052"));
lf[61]=C_h_intern(&lf[61],1, C_text("&"));
lf[62]=C_h_intern(&lf[62],3, C_text("tap"));
lf[63]=C_h_intern(&lf[63],6, C_text("\342\210\247\342\210\247"));
lf[64]=C_h_intern(&lf[64],6, C_text("\342\210\250\342\210\250"));
lf[65]=C_h_intern(&lf[65],3, C_text("\342\210\210"));
lf[66]=C_h_intern(&lf[66],5, C_text("right"));
lf[67]=C_h_intern(&lf[67],4, C_text("left"));
lf[68]=C_h_intern(&lf[68],3, C_text("\342\212\231"));
lf[69]=C_decode_literal(C_heaptop,C_text("\376B\000\000\017key not found: "));
lf[70]=C_h_intern(&lf[70],12, C_text("srfi-1#assoc"));
lf[71]=C_h_intern(&lf[71],4, C_text("conj"));
lf[72]=C_h_intern(&lf[72],3, C_text("\342\212\203"));
lf[73]=C_h_intern(&lf[73],4, C_text("\342\206\221n"));
lf[74]=C_h_intern(&lf[74],11, C_text("srfi-1#take"));
lf[75]=C_h_intern(&lf[75],4, C_text("\342\206\223n"));
lf[76]=C_h_intern(&lf[76],11, C_text("srfi-1#drop"));
lf[77]=C_h_intern(&lf[77],3, C_text("\342\215\213"));
lf[78]=C_h_intern(&lf[78],17, C_text("chicken.sort#sort"));
lf[79]=C_h_intern(&lf[79],6, C_text("right\077"));
lf[80]=C_h_intern(&lf[80],11, C_text("right-value"));
lf[81]=C_h_intern(&lf[81],5, C_text("left\077"));
lf[82]=C_h_intern(&lf[82],10, C_text("left-value"));
lf[83]=C_h_intern(&lf[83],2, C_text("_+"));
lf[84]=C_h_intern(&lf[84],2, C_text("+_"));
lf[85]=C_h_intern(&lf[85],3, C_text("_+\077"));
lf[86]=C_h_intern(&lf[86],3, C_text("+_\077"));
lf[87]=C_h_intern(&lf[87],7, C_text("either\077"));
lf[88]=C_h_intern(&lf[88],12, C_text("either-guard"));
lf[89]=C_h_intern(&lf[89],18, C_text("chicken.base#error"));
lf[90]=C_decode_literal(C_heaptop,C_text("\376B\000\000\012not either"));
lf[91]=C_h_intern(&lf[91],4, C_text("gett"));
lf[92]=C_h_intern(&lf[92],3, C_text("get"));
lf[93]=C_h_intern(&lf[93],6, C_text("ensure"));
lf[94]=C_h_intern(&lf[94],5, C_text("fmapp"));
lf[95]=C_h_intern(&lf[95],4, C_text("fmap"));
lf[96]=C_h_intern(&lf[96],5, C_text("bindd"));
lf[97]=C_h_intern(&lf[97],4, C_text("bind"));
lf[98]=C_h_intern(&lf[98],6, C_text("fjoinn"));
lf[99]=C_h_intern(&lf[99],5, C_text("fjoin"));
lf[100]=C_h_intern(&lf[100],3, C_text("\342\212\245"));
lf[101]=C_h_intern(&lf[101],3, C_text(">>="));
lf[102]=C_h_intern(&lf[102],5, C_text("thenn"));
lf[103]=C_h_intern(&lf[103],4, C_text("then"));
lf[104]=C_h_intern(&lf[104],3, C_text("ass"));
lf[105]=C_h_intern(&lf[105],2, C_text("as"));
lf[106]=C_h_intern(&lf[106],2, C_text("\052>"));
lf[107]=C_h_intern(&lf[107],2, C_text("$>"));
lf[108]=C_h_intern(&lf[108],6, C_text("lift22"));
lf[109]=C_h_intern(&lf[109],5, C_text("lift2"));
lf[110]=C_h_intern(&lf[110],9, C_text("fcomposee"));
lf[111]=C_h_intern(&lf[111],8, C_text("fcompose"));
lf[112]=C_h_intern(&lf[112],8, C_text("kleislii"));
lf[113]=C_h_intern(&lf[113],7, C_text("kleisli"));
lf[114]=C_h_intern(&lf[114],3, C_text("\342\227\201"));
lf[115]=C_h_intern(&lf[115],3, C_text("\342\227\200"));
lf[116]=C_h_intern(&lf[116],10, C_text("break-left"));
lf[117]=C_h_intern(&lf[117],5, C_text("\342\226\263+_"));
lf[118]=C_h_intern(&lf[118],9, C_text("sequencee"));
lf[119]=C_h_intern(&lf[119],20, C_text("chicken.base#call/cc"));
lf[120]=C_h_intern(&lf[120],8, C_text("sequence"));
lf[121]=C_h_intern(&lf[121],9, C_text("traversee"));
lf[122]=C_h_intern(&lf[122],8, C_text("traverse"));
lf[123]=C_h_intern(&lf[123],5, C_text("lmapp"));
lf[124]=C_h_intern(&lf[124],4, C_text("lmap"));
lf[125]=C_h_intern(&lf[125],4, C_text("\342\212\231_"));
lf[126]=C_h_intern(&lf[126],13, C_text("split-choicee"));
lf[127]=C_h_intern(&lf[127],12, C_text("split-choice"));
lf[128]=C_h_intern(&lf[128],7, C_text("fan-inn"));
lf[129]=C_h_intern(&lf[129],6, C_text("fan-in"));
lf[130]=C_h_intern(&lf[130],3, C_text("+++"));
lf[131]=C_h_intern(&lf[131],3, C_text("///"));
lf[132]=C_h_intern(&lf[132],11, C_text("alternative"));
lf[133]=C_h_intern(&lf[133],3, C_text("</>"));
lf[134]=C_h_intern(&lf[134],2, C_text("\316\271"));
lf[135]=C_h_intern(&lf[135],9, C_text("condition"));
lf[136]=C_h_intern(&lf[136],3, C_text("exn"));
lf[137]=C_h_intern(&lf[137],40, C_text("chicken.condition#get-condition-property"));
lf[138]=C_h_intern(&lf[138],7, C_text("message"));
lf[139]=C_h_intern(&lf[139],24, C_text("chicken.condition#signal"));
lf[140]=C_h_intern(&lf[140],40, C_text("chicken.condition#with-exception-handler"));
lf[141]=C_h_intern(&lf[141],37, C_text("scheme#call-with-current-continuation"));
lf[142]=C_h_intern(&lf[142],34, C_text("chicken.base#implicit-exit-handler"));
lf[143]=C_h_intern(&lf[143],11, C_text("scheme#read"));
lf[144]=C_h_intern(&lf[144],42, C_text("chicken.read-syntax#set-sharp-read-syntax!"));
lf[145]=C_h_intern(&lf[145],7, C_text("fprintf"));
lf[146]=C_h_intern(&lf[146],18, C_text("##sys#write-char-0"));
lf[147]=C_h_intern(&lf[147],11, C_text("##sys#print"));
lf[148]=C_decode_literal(C_heaptop,C_text("\376B\000\000\003#L("));
lf[149]=C_h_intern(&lf[149],32, C_text("chicken.base#set-record-printer!"));
lf[150]=C_decode_literal(C_heaptop,C_text("\376B\000\000\003#R("));
lf[151]=C_h_intern(&lf[151],27, C_text("chicken.load#load-extension"));
lf[152]=C_h_intern(&lf[152],6, C_text("srfi-1"));
C_register_lf2(lf,153,create_ptable());{}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1211,a[2]=t1,tmp=(C_word)a,a+=3,tmp);{
C_word *av2=av;
av2[0]=C_SCHEME_UNDEFINED;
av2[1]=t2;
C_library_toplevel(2,av2);}}

#ifdef C_ENABLE_PTABLES
static C_PTABLE_ENTRY ptable[163] = {
{C_text("f2176:analysis_2fhelpers_2fprelude_2escm"),(void*)f2176},
{C_text("f2181:analysis_2fhelpers_2fprelude_2escm"),(void*)f2181},
{C_text("f2186:analysis_2fhelpers_2fprelude_2escm"),(void*)f2186},
{C_text("f2233:analysis_2fhelpers_2fprelude_2escm"),(void*)f2233},
{C_text("f2236:analysis_2fhelpers_2fprelude_2escm"),(void*)f2236},
{C_text("f2246:analysis_2fhelpers_2fprelude_2escm"),(void*)f2246},
{C_text("f_1211:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1211},
{C_text("f_1214:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1214},
{C_text("f_1217:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1217},
{C_text("f_1220:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1220},
{C_text("f_1223:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1223},
{C_text("f_1226:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1226},
{C_text("f_1244:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1244},
{C_text("f_1258:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1258},
{C_text("f_1261:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1261},
{C_text("f_1263:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1263},
{C_text("f_1266:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1266},
{C_text("f_1268:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1268},
{C_text("f_1270:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1270},
{C_text("f_1276:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1276},
{C_text("f_1278:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1278},
{C_text("f_1286:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1286},
{C_text("f_1292:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1292},
{C_text("f_1294:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1294},
{C_text("f_1302:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1302},
{C_text("f_1304:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1304},
{C_text("f_1306:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1306},
{C_text("f_1314:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1314},
{C_text("f_1316:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1316},
{C_text("f_1318:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1318},
{C_text("f_1326:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1326},
{C_text("f_1330:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1330},
{C_text("f_1332:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1332},
{C_text("f_1334:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1334},
{C_text("f_1342:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1342},
{C_text("f_1346:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1346},
{C_text("f_1348:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1348},
{C_text("f_1350:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1350},
{C_text("f_1358:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1358},
{C_text("f_1362:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1362},
{C_text("f_1364:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1364},
{C_text("f_1366:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1366},
{C_text("f_1381:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1381},
{C_text("f_1383:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1383},
{C_text("f_1385:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1385},
{C_text("f_1393:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1393},
{C_text("f_1395:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1395},
{C_text("f_1399:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1399},
{C_text("f_1401:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1401},
{C_text("f_1407:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1407},
{C_text("f_1413:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1413},
{C_text("f_1417:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1417},
{C_text("f_1428:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1428},
{C_text("f_1430:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1430},
{C_text("f_1441:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1441},
{C_text("f_1459:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1459},
{C_text("f_1477:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1477},
{C_text("f_1483:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1483},
{C_text("f_1489:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1489},
{C_text("f_1495:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1495},
{C_text("f_1505:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1505},
{C_text("f_1508:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1508},
{C_text("f_1510:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1510},
{C_text("f_1516:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1516},
{C_text("f_1522:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1522},
{C_text("f_1532:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1532},
{C_text("f_1535:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1535},
{C_text("f_1541:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1541},
{C_text("f_1545:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1545},
{C_text("f_1553:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1553},
{C_text("f_1560:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1560},
{C_text("f_1568:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1568},
{C_text("f_1575:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1575},
{C_text("f_1585:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1585},
{C_text("f_1589:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1589},
{C_text("f_1591:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1591},
{C_text("f_1603:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1603},
{C_text("f_1610:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1610},
{C_text("f_1617:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1617},
{C_text("f_1621:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1621},
{C_text("f_1623:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1623},
{C_text("f_1631:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1631},
{C_text("f_1633:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1633},
{C_text("f_1640:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1640},
{C_text("f_1647:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1647},
{C_text("f_1649:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1649},
{C_text("f_1657:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1657},
{C_text("f_1659:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1659},
{C_text("f_1665:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1665},
{C_text("f_1674:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1674},
{C_text("f_1684:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1684},
{C_text("f_1692:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1692},
{C_text("f_1694:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1694},
{C_text("f_1704:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1704},
{C_text("f_1716:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1716},
{C_text("f_1722:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1722},
{C_text("f_1728:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1728},
{C_text("f_1734:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1734},
{C_text("f_1740:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1740},
{C_text("f_1746:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1746},
{C_text("f_1752:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1752},
{C_text("f_1758:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1758},
{C_text("f_1764:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1764},
{C_text("f_1770:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1770},
{C_text("f_1778:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1778},
{C_text("f_1785:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1785},
{C_text("f_1791:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1791},
{C_text("f_1797:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1797},
{C_text("f_1803:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1803},
{C_text("f_1811:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1811},
{C_text("f_1817:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1817},
{C_text("f_1823:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1823},
{C_text("f_1829:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1829},
{C_text("f_1837:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1837},
{C_text("f_1843:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1843},
{C_text("f_1851:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1851},
{C_text("f_1853:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1853},
{C_text("f_1861:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1861},
{C_text("f_1863:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1863},
{C_text("f_1870:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1870},
{C_text("f_1877:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1877},
{C_text("f_1881:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1881},
{C_text("f_1883:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1883},
{C_text("f_1891:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1891},
{C_text("f_1894:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1894},
{C_text("f_1902:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1902},
{C_text("f_1904:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1904},
{C_text("f_1912:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1912},
{C_text("f_1914:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1914},
{C_text("f_1921:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1921},
{C_text("f_1928:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1928},
{C_text("f_1935:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1935},
{C_text("f_1937:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1937},
{C_text("f_1945:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1945},
{C_text("f_1949:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1949},
{C_text("f_1955:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1955},
{C_text("f_1961:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1961},
{C_text("f_1968:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1968},
{C_text("f_1974:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1974},
{C_text("f_1978:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1978},
{C_text("f_1981:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1981},
{C_text("f_1992:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1992},
{C_text("f_1998:analysis_2fhelpers_2fprelude_2escm"),(void*)f_1998},
{C_text("f_2004:analysis_2fhelpers_2fprelude_2escm"),(void*)f_2004},
{C_text("f_2021:analysis_2fhelpers_2fprelude_2escm"),(void*)f_2021},
{C_text("f_2035:analysis_2fhelpers_2fprelude_2escm"),(void*)f_2035},
{C_text("f_2037:analysis_2fhelpers_2fprelude_2escm"),(void*)f_2037},
{C_text("f_2039:analysis_2fhelpers_2fprelude_2escm"),(void*)f_2039},
{C_text("f_2047:analysis_2fhelpers_2fprelude_2escm"),(void*)f_2047},
{C_text("f_2053:analysis_2fhelpers_2fprelude_2escm"),(void*)f_2053},
{C_text("f_2070:analysis_2fhelpers_2fprelude_2escm"),(void*)f_2070},
{C_text("f_2072:analysis_2fhelpers_2fprelude_2escm"),(void*)f_2072},
{C_text("f_2084:analysis_2fhelpers_2fprelude_2escm"),(void*)f_2084},
{C_text("f_2086:analysis_2fhelpers_2fprelude_2escm"),(void*)f_2086},
{C_text("f_2093:analysis_2fhelpers_2fprelude_2escm"),(void*)f_2093},
{C_text("f_2096:analysis_2fhelpers_2fprelude_2escm"),(void*)f_2096},
{C_text("f_2105:analysis_2fhelpers_2fprelude_2escm"),(void*)f_2105},
{C_text("f_2117:analysis_2fhelpers_2fprelude_2escm"),(void*)f_2117},
{C_text("f_2119:analysis_2fhelpers_2fprelude_2escm"),(void*)f_2119},
{C_text("f_2126:analysis_2fhelpers_2fprelude_2escm"),(void*)f_2126},
{C_text("f_2129:analysis_2fhelpers_2fprelude_2escm"),(void*)f_2129},
{C_text("toplevel:analysis_2fhelpers_2fprelude_2escm"),(void*)C_toplevel},
{NULL,NULL}};
#endif

static C_PTABLE_ENTRY *create_ptable(void){
#ifdef C_ENABLE_PTABLES
return ptable;
#else
return NULL;
#endif
}

/*
S|applied compiler syntax:
S|  chicken.format#fprintf		2
o|eliminated procedure checks: 17 
o|specializations:
o|  1 (##sys#call-with-values (procedure () *) *)
o|  2 (##sys#check-output-port * * *)
(o e)|safe calls: 112 
(o e)|assignments to immediate values: 1 
o|dropping redundant toplevel assignment: right 
o|dropping redundant toplevel assignment: left 
o|inlining procedure: k1403 
o|inlining procedure: k1403 
o|inlining procedure: k1409 
o|inlining procedure: k1409 
o|inlining procedure: k1446 
o|inlining procedure: k1446 
o|inlining procedure: k1464 
o|inlining procedure: k1464 
o|inlining procedure: k1546 
o|inlining procedure: k1546 
o|inlining procedure: k1555 
o|inlining procedure: k1555 
o|inlining procedure: k1570 
o|inlining procedure: k1570 
o|inlining procedure: k1593 
o|inlining procedure: "(analysis/helpers/prelude.scm:78) right" 
o|inlining procedure: k1593 
o|inlining procedure: "(analysis/helpers/prelude.scm:78) left" 
o|inlining procedure: k1605 
o|inlining procedure: "(analysis/helpers/prelude.scm:79) right" 
o|inlining procedure: k1605 
o|inlining procedure: k1635 
o|inlining procedure: k1635 
o|inlining procedure: "(analysis/helpers/prelude.scm:86) K" 
o|inlining procedure: "(analysis/helpers/prelude.scm:88) K" 
o|inlining procedure: "(analysis/helpers/prelude.scm:89) K" 
o|inlining procedure: k1780 
o|inlining procedure: k1780 
o|inlining procedure: "(analysis/helpers/prelude.scm:101) right" 
o|propagated global variable: value4162190 ∅ 
o|inlining procedure: "(analysis/helpers/prelude.scm:103) right" 
o|propagated global variable: value4162195 ∅ 
o|inlining procedure: k1865 
o|inlining procedure: "(analysis/helpers/prelude.scm:106) left" 
o|inlining procedure: k1865 
o|inlining procedure: k1916 
o|inlining procedure: k1916 
o|inlining procedure: k1963 
o|inlining procedure: k1963 
o|inlining procedure: k1982 
o|inlining procedure: k1982 
o|inlining procedure: "(analysis/helpers/prelude.scm:116) right" 
o|inlining procedure: "(analysis/helpers/prelude.scm:116) left?" 
o|inlining procedure: k2009 
o|inlining procedure: "(analysis/helpers/prelude.scm:116) left" 
o|inlining procedure: k2009 
o|merged explicitly consed rest parameter: args858875 
o|consed rest parameter at call site: tmp21206 1 
o|substituted constant variable: a2089 
o|substituted constant variable: a2090 
o|substituted constant variable: a2122 
o|substituted constant variable: a2123 
o|replaced variables: 161 
o|removed binding forms: 123 
o|substituted constant variable: r14042138 
o|replaced variables: 41 
o|removed binding forms: 167 
o|inlining procedure: "(analysis/helpers/prelude.scm:26) C" 
o|inlining procedure: k1680 
o|inlining procedure: k1700 
o|inlining procedure: k1710 
o|removed binding forms: 42 
o|inlining procedure: "(analysis/helpers/prelude.scm:32) ensure" 
o|replaced variables: 2 
o|removed binding forms: 3 
o|inlining procedure: k1372 
o|replaced variables: 5 
o|removed binding forms: 2 
o|removed binding forms: 5 
o|simplifications: ((if . 7) (##core#call . 38)) 
o|  call simplifications:
o|    scheme#car	2
o|    ##sys#apply
o|    scheme#list-ref
o|    ##sys#slot
o|    scheme#memv
o|    ##sys#check-structure	2
o|    ##sys#block-ref	2
o|    ##sys#structure?	4
o|    scheme#length	2
o|    scheme#>	2
o|    ##sys#make-structure	12
o|    scheme#map	3
o|    ##sys#list	4
o|    scheme#apply
o|contracted procedure: k1288 
o|contracted procedure: k1422 
o|contracted procedure: k1436 
o|contracted procedure: k1443 
o|contracted procedure: k1449 
o|contracted procedure: k1461 
o|contracted procedure: k1467 
o|contracted procedure: k1497 
o|contracted procedure: k1524 
o|contracted procedure: k1813 
o|contracted procedure: k1839 
o|contracted procedure: k1985 
o|contracted procedure: k2028 
o|contracted procedure: k2006 
o|contracted procedure: k2012 
o|contracted procedure: k2078 
o|contracted procedure: k2111 
o|simplifications: ((let . 2)) 
o|removed binding forms: 17 
o|inlining procedure: "(analysis/helpers/prelude.scm:54) left-value" 
o|inlining procedure: "(analysis/helpers/prelude.scm:44) right-value" 
o|replaced variables: 6 
o|inlining procedure: k2101 
o|inlining procedure: k2134 
o|removed binding forms: 4 
o|simplifications: ((let . 2)) 
o|removed binding forms: 2 
o|direct leaf routine/allocation: g872873 0 
o|direct leaf routine with hoistable closures/allocation: tmp11205 (g872873) 0 
o|contracted procedure: k2062 
o|removed binding forms: 2 
o|customizable procedures: (tmp21206 r13732244) 
o|calls to known targets: 29 
*/
/* end of file */
