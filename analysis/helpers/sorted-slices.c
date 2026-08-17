/* Generated from analysis/helpers/sorted-slices.scm by the CHICKEN compiler
   http://www.call-cc.org
   Version 5.4.0 (rev 1a1d1495)
   openbsd-unix-clang-x86-64 [ 64bit dload ]
   command line: analysis/helpers/sorted-slices.scm -output-file analysis/helpers/sorted-slices.c -optimize-level 3 -lfa2
   uses: eval srfi-4 data-structures extras library
*/
#include "chicken.h"

static C_PTABLE_ENTRY *create_ptable(void);
C_noret_decl(C_eval_toplevel)
C_externimport void C_ccall C_eval_toplevel(C_word c,C_word *av) C_noret;
C_noret_decl(C_srfi_2d4_toplevel)
C_externimport void C_ccall C_srfi_2d4_toplevel(C_word c,C_word *av) C_noret;
C_noret_decl(C_data_2dstructures_toplevel)
C_externimport void C_ccall C_data_2dstructures_toplevel(C_word c,C_word *av) C_noret;
C_noret_decl(C_extras_toplevel)
C_externimport void C_ccall C_extras_toplevel(C_word c,C_word *av) C_noret;
C_noret_decl(C_library_toplevel)
C_externimport void C_ccall C_library_toplevel(C_word c,C_word *av) C_noret;

static C_TLS C_word lf[92];
static double C_possibly_force_alignment;
static C_char C_TLS li0[] C_aligned={C_lihdr(0,0,29),40,109,97,107,101,45,115,108,105,99,101,32,108,101,110,103,116,104,32,115,111,114,116,115,32,118,101,99,41,0,0,0};
static C_char C_TLS li1[] C_aligned={C_lihdr(0,0,10),40,115,108,105,99,101,63,32,120,41,0,0,0,0,0,0};
static C_char C_TLS li2[] C_aligned={C_lihdr(0,0,16),40,115,108,105,99,101,45,108,101,110,103,116,104,32,120,41};
static C_char C_TLS li3[] C_aligned={C_lihdr(0,0,23),40,115,108,105,99,101,45,108,101,110,103,116,104,45,115,101,116,33,32,120,32,121,41,0};
static C_char C_TLS li4[] C_aligned={C_lihdr(0,0,15),40,115,108,105,99,101,45,115,111,114,116,115,32,120,41,0};
static C_char C_TLS li5[] C_aligned={C_lihdr(0,0,22),40,115,108,105,99,101,45,115,111,114,116,115,45,115,101,116,33,32,120,32,121,41,0,0};
static C_char C_TLS li6[] C_aligned={C_lihdr(0,0,13),40,115,108,105,99,101,45,118,101,99,32,120,41,0,0,0};
static C_char C_TLS li7[] C_aligned={C_lihdr(0,0,20),40,115,108,105,99,101,45,118,101,99,45,115,101,116,33,32,120,32,121,41,0,0,0,0};
static C_char C_TLS li8[] C_aligned={C_lihdr(0,0,15),40,97,49,49,53,57,32,207,137,32,112,111,114,116,41,0};
static C_char C_TLS li9[] C_aligned={C_lihdr(0,0,10),40,116,111,112,108,101,118,101,108,41,0,0,0,0,0,0};


C_noret_decl(f_1003)
static void C_ccall f_1003(C_word c,C_word *av) C_noret;
C_noret_decl(f_1007)
static void C_ccall f_1007(C_word c,C_word *av) C_noret;
C_noret_decl(f_1015)
static void C_ccall f_1015(C_word c,C_word *av) C_noret;
C_noret_decl(f_1023)
static void C_ccall f_1023(C_word c,C_word *av) C_noret;
C_noret_decl(f_1027)
static void C_ccall f_1027(C_word c,C_word *av) C_noret;
C_noret_decl(f_1031)
static void C_ccall f_1031(C_word c,C_word *av) C_noret;
C_noret_decl(f_1035)
static void C_ccall f_1035(C_word c,C_word *av) C_noret;
C_noret_decl(f_1039)
static void C_ccall f_1039(C_word c,C_word *av) C_noret;
C_noret_decl(f_1043)
static void C_ccall f_1043(C_word c,C_word *av) C_noret;
C_noret_decl(f_1047)
static void C_ccall f_1047(C_word c,C_word *av) C_noret;
C_noret_decl(f_1055)
static void C_ccall f_1055(C_word c,C_word *av) C_noret;
C_noret_decl(f_1059)
static void C_ccall f_1059(C_word c,C_word *av) C_noret;
C_noret_decl(f_1063)
static void C_ccall f_1063(C_word c,C_word *av) C_noret;
C_noret_decl(f_1067)
static void C_ccall f_1067(C_word c,C_word *av) C_noret;
C_noret_decl(f_1071)
static void C_ccall f_1071(C_word c,C_word *av) C_noret;
C_noret_decl(f_1075)
static void C_ccall f_1075(C_word c,C_word *av) C_noret;
C_noret_decl(f_1078)
static void C_ccall f_1078(C_word c,C_word *av) C_noret;
C_noret_decl(f_1086)
static void C_ccall f_1086(C_word c,C_word *av) C_noret;
C_noret_decl(f_1090)
static void C_ccall f_1090(C_word c,C_word *av) C_noret;
C_noret_decl(f_1094)
static void C_ccall f_1094(C_word c,C_word *av) C_noret;
C_noret_decl(f_1098)
static void C_ccall f_1098(C_word c,C_word *av) C_noret;
C_noret_decl(f_1102)
static void C_ccall f_1102(C_word c,C_word *av) C_noret;
C_noret_decl(f_1106)
static void C_ccall f_1106(C_word c,C_word *av) C_noret;
C_noret_decl(f_1160)
static void C_ccall f_1160(C_word c,C_word *av) C_noret;
C_noret_decl(f_1167)
static void C_ccall f_1167(C_word c,C_word *av) C_noret;
C_noret_decl(f_1170)
static void C_ccall f_1170(C_word c,C_word *av) C_noret;
C_noret_decl(f_1177)
static void C_ccall f_1177(C_word c,C_word *av) C_noret;
C_noret_decl(f_1181)
static void C_ccall f_1181(C_word c,C_word *av) C_noret;
C_noret_decl(f_1185)
static void C_ccall f_1185(C_word c,C_word *av) C_noret;
C_noret_decl(f_333)
static void C_ccall f_333(C_word c,C_word *av) C_noret;
C_noret_decl(f_336)
static void C_ccall f_336(C_word c,C_word *av) C_noret;
C_noret_decl(f_339)
static void C_ccall f_339(C_word c,C_word *av) C_noret;
C_noret_decl(f_342)
static void C_ccall f_342(C_word c,C_word *av) C_noret;
C_noret_decl(f_345)
static void C_ccall f_345(C_word c,C_word *av) C_noret;
C_noret_decl(f_348)
static void C_ccall f_348(C_word c,C_word *av) C_noret;
C_noret_decl(f_351)
static void C_ccall f_351(C_word c,C_word *av) C_noret;
C_noret_decl(f_357)
static void C_ccall f_357(C_word c,C_word *av) C_noret;
C_noret_decl(f_363)
static void C_ccall f_363(C_word c,C_word *av) C_noret;
C_noret_decl(f_372)
static void C_ccall f_372(C_word c,C_word *av) C_noret;
C_noret_decl(f_381)
static void C_ccall f_381(C_word c,C_word *av) C_noret;
C_noret_decl(f_390)
static void C_ccall f_390(C_word c,C_word *av) C_noret;
C_noret_decl(f_399)
static void C_ccall f_399(C_word c,C_word *av) C_noret;
C_noret_decl(f_408)
static void C_ccall f_408(C_word c,C_word *av) C_noret;
C_noret_decl(f_418)
static void C_ccall f_418(C_word c,C_word *av) C_noret;
C_noret_decl(f_421)
static void C_ccall f_421(C_word c,C_word *av) C_noret;
C_noret_decl(f_424)
static void C_ccall f_424(C_word c,C_word *av) C_noret;
C_noret_decl(f_427)
static void C_ccall f_427(C_word c,C_word *av) C_noret;
C_noret_decl(f_430)
static void C_ccall f_430(C_word c,C_word *av) C_noret;
C_noret_decl(f_433)
static void C_ccall f_433(C_word c,C_word *av) C_noret;
C_noret_decl(f_436)
static void C_ccall f_436(C_word c,C_word *av) C_noret;
C_noret_decl(f_439)
static void C_ccall f_439(C_word c,C_word *av) C_noret;
C_noret_decl(f_442)
static void C_ccall f_442(C_word c,C_word *av) C_noret;
C_noret_decl(f_445)
static void C_ccall f_445(C_word c,C_word *av) C_noret;
C_noret_decl(f_448)
static void C_ccall f_448(C_word c,C_word *av) C_noret;
C_noret_decl(f_451)
static void C_ccall f_451(C_word c,C_word *av) C_noret;
C_noret_decl(f_454)
static void C_ccall f_454(C_word c,C_word *av) C_noret;
C_noret_decl(f_457)
static void C_ccall f_457(C_word c,C_word *av) C_noret;
C_noret_decl(f_460)
static void C_ccall f_460(C_word c,C_word *av) C_noret;
C_noret_decl(f_463)
static void C_ccall f_463(C_word c,C_word *av) C_noret;
C_noret_decl(f_466)
static void C_ccall f_466(C_word c,C_word *av) C_noret;
C_noret_decl(f_469)
static void C_ccall f_469(C_word c,C_word *av) C_noret;
C_noret_decl(f_472)
static void C_ccall f_472(C_word c,C_word *av) C_noret;
C_noret_decl(f_475)
static void C_ccall f_475(C_word c,C_word *av) C_noret;
C_noret_decl(f_478)
static void C_ccall f_478(C_word c,C_word *av) C_noret;
C_noret_decl(f_481)
static void C_ccall f_481(C_word c,C_word *av) C_noret;
C_noret_decl(f_484)
static void C_ccall f_484(C_word c,C_word *av) C_noret;
C_noret_decl(f_487)
static void C_ccall f_487(C_word c,C_word *av) C_noret;
C_noret_decl(f_490)
static void C_ccall f_490(C_word c,C_word *av) C_noret;
C_noret_decl(f_493)
static void C_ccall f_493(C_word c,C_word *av) C_noret;
C_noret_decl(f_496)
static void C_ccall f_496(C_word c,C_word *av) C_noret;
C_noret_decl(f_499)
static void C_ccall f_499(C_word c,C_word *av) C_noret;
C_noret_decl(f_502)
static void C_ccall f_502(C_word c,C_word *av) C_noret;
C_noret_decl(f_505)
static void C_ccall f_505(C_word c,C_word *av) C_noret;
C_noret_decl(f_508)
static void C_ccall f_508(C_word c,C_word *av) C_noret;
C_noret_decl(f_511)
static void C_ccall f_511(C_word c,C_word *av) C_noret;
C_noret_decl(f_514)
static void C_ccall f_514(C_word c,C_word *av) C_noret;
C_noret_decl(f_517)
static void C_ccall f_517(C_word c,C_word *av) C_noret;
C_noret_decl(f_520)
static void C_ccall f_520(C_word c,C_word *av) C_noret;
C_noret_decl(f_526)
static void C_ccall f_526(C_word c,C_word *av) C_noret;
C_noret_decl(f_530)
static void C_ccall f_530(C_word c,C_word *av) C_noret;
C_noret_decl(f_534)
static void C_ccall f_534(C_word c,C_word *av) C_noret;
C_noret_decl(f_538)
static void C_ccall f_538(C_word c,C_word *av) C_noret;
C_noret_decl(f_542)
static void C_ccall f_542(C_word c,C_word *av) C_noret;
C_noret_decl(f_546)
static void C_ccall f_546(C_word c,C_word *av) C_noret;
C_noret_decl(f_550)
static void C_ccall f_550(C_word c,C_word *av) C_noret;
C_noret_decl(f_554)
static void C_ccall f_554(C_word c,C_word *av) C_noret;
C_noret_decl(f_558)
static void C_ccall f_558(C_word c,C_word *av) C_noret;
C_noret_decl(f_562)
static void C_ccall f_562(C_word c,C_word *av) C_noret;
C_noret_decl(f_566)
static void C_ccall f_566(C_word c,C_word *av) C_noret;
C_noret_decl(f_570)
static void C_ccall f_570(C_word c,C_word *av) C_noret;
C_noret_decl(f_574)
static void C_ccall f_574(C_word c,C_word *av) C_noret;
C_noret_decl(f_578)
static void C_ccall f_578(C_word c,C_word *av) C_noret;
C_noret_decl(f_582)
static void C_ccall f_582(C_word c,C_word *av) C_noret;
C_noret_decl(f_586)
static void C_ccall f_586(C_word c,C_word *av) C_noret;
C_noret_decl(f_590)
static void C_ccall f_590(C_word c,C_word *av) C_noret;
C_noret_decl(f_594)
static void C_ccall f_594(C_word c,C_word *av) C_noret;
C_noret_decl(f_597)
static void C_ccall f_597(C_word c,C_word *av) C_noret;
C_noret_decl(f_601)
static void C_ccall f_601(C_word c,C_word *av) C_noret;
C_noret_decl(f_611)
static void C_ccall f_611(C_word c,C_word *av) C_noret;
C_noret_decl(f_615)
static void C_ccall f_615(C_word c,C_word *av) C_noret;
C_noret_decl(f_619)
static void C_ccall f_619(C_word c,C_word *av) C_noret;
C_noret_decl(f_627)
static void C_ccall f_627(C_word c,C_word *av) C_noret;
C_noret_decl(f_631)
static void C_ccall f_631(C_word c,C_word *av) C_noret;
C_noret_decl(f_634)
static void C_ccall f_634(C_word c,C_word *av) C_noret;
C_noret_decl(f_638)
static void C_ccall f_638(C_word c,C_word *av) C_noret;
C_noret_decl(f_646)
static void C_ccall f_646(C_word c,C_word *av) C_noret;
C_noret_decl(f_650)
static void C_ccall f_650(C_word c,C_word *av) C_noret;
C_noret_decl(f_654)
static void C_ccall f_654(C_word c,C_word *av) C_noret;
C_noret_decl(f_658)
static void C_ccall f_658(C_word c,C_word *av) C_noret;
C_noret_decl(f_662)
static void C_ccall f_662(C_word c,C_word *av) C_noret;
C_noret_decl(f_666)
static void C_ccall f_666(C_word c,C_word *av) C_noret;
C_noret_decl(f_670)
static void C_ccall f_670(C_word c,C_word *av) C_noret;
C_noret_decl(f_673)
static void C_ccall f_673(C_word c,C_word *av) C_noret;
C_noret_decl(f_677)
static void C_ccall f_677(C_word c,C_word *av) C_noret;
C_noret_decl(f_689)
static void C_ccall f_689(C_word c,C_word *av) C_noret;
C_noret_decl(f_693)
static void C_ccall f_693(C_word c,C_word *av) C_noret;
C_noret_decl(f_697)
static void C_ccall f_697(C_word c,C_word *av) C_noret;
C_noret_decl(f_701)
static void C_ccall f_701(C_word c,C_word *av) C_noret;
C_noret_decl(f_705)
static void C_ccall f_705(C_word c,C_word *av) C_noret;
C_noret_decl(f_709)
static void C_ccall f_709(C_word c,C_word *av) C_noret;
C_noret_decl(f_713)
static void C_ccall f_713(C_word c,C_word *av) C_noret;
C_noret_decl(f_717)
static void C_ccall f_717(C_word c,C_word *av) C_noret;
C_noret_decl(f_721)
static void C_ccall f_721(C_word c,C_word *av) C_noret;
C_noret_decl(f_725)
static void C_ccall f_725(C_word c,C_word *av) C_noret;
C_noret_decl(f_729)
static void C_ccall f_729(C_word c,C_word *av) C_noret;
C_noret_decl(f_733)
static void C_ccall f_733(C_word c,C_word *av) C_noret;
C_noret_decl(f_737)
static void C_ccall f_737(C_word c,C_word *av) C_noret;
C_noret_decl(f_741)
static void C_ccall f_741(C_word c,C_word *av) C_noret;
C_noret_decl(f_745)
static void C_ccall f_745(C_word c,C_word *av) C_noret;
C_noret_decl(f_749)
static void C_ccall f_749(C_word c,C_word *av) C_noret;
C_noret_decl(f_761)
static void C_ccall f_761(C_word c,C_word *av) C_noret;
C_noret_decl(f_765)
static void C_ccall f_765(C_word c,C_word *av) C_noret;
C_noret_decl(f_769)
static void C_ccall f_769(C_word c,C_word *av) C_noret;
C_noret_decl(f_772)
static void C_ccall f_772(C_word c,C_word *av) C_noret;
C_noret_decl(f_780)
static void C_ccall f_780(C_word c,C_word *av) C_noret;
C_noret_decl(f_784)
static void C_ccall f_784(C_word c,C_word *av) C_noret;
C_noret_decl(f_788)
static void C_ccall f_788(C_word c,C_word *av) C_noret;
C_noret_decl(f_792)
static void C_ccall f_792(C_word c,C_word *av) C_noret;
C_noret_decl(f_796)
static void C_ccall f_796(C_word c,C_word *av) C_noret;
C_noret_decl(f_800)
static void C_ccall f_800(C_word c,C_word *av) C_noret;
C_noret_decl(f_804)
static void C_ccall f_804(C_word c,C_word *av) C_noret;
C_noret_decl(f_808)
static void C_ccall f_808(C_word c,C_word *av) C_noret;
C_noret_decl(f_812)
static void C_ccall f_812(C_word c,C_word *av) C_noret;
C_noret_decl(f_816)
static void C_ccall f_816(C_word c,C_word *av) C_noret;
C_noret_decl(f_820)
static void C_ccall f_820(C_word c,C_word *av) C_noret;
C_noret_decl(f_823)
static void C_ccall f_823(C_word c,C_word *av) C_noret;
C_noret_decl(f_827)
static void C_ccall f_827(C_word c,C_word *av) C_noret;
C_noret_decl(f_831)
static void C_ccall f_831(C_word c,C_word *av) C_noret;
C_noret_decl(f_835)
static void C_ccall f_835(C_word c,C_word *av) C_noret;
C_noret_decl(f_839)
static void C_ccall f_839(C_word c,C_word *av) C_noret;
C_noret_decl(f_847)
static void C_ccall f_847(C_word c,C_word *av) C_noret;
C_noret_decl(f_851)
static void C_ccall f_851(C_word c,C_word *av) C_noret;
C_noret_decl(f_855)
static void C_ccall f_855(C_word c,C_word *av) C_noret;
C_noret_decl(f_859)
static void C_ccall f_859(C_word c,C_word *av) C_noret;
C_noret_decl(f_863)
static void C_ccall f_863(C_word c,C_word *av) C_noret;
C_noret_decl(f_866)
static void C_ccall f_866(C_word c,C_word *av) C_noret;
C_noret_decl(f_870)
static void C_ccall f_870(C_word c,C_word *av) C_noret;
C_noret_decl(f_878)
static void C_ccall f_878(C_word c,C_word *av) C_noret;
C_noret_decl(f_882)
static void C_ccall f_882(C_word c,C_word *av) C_noret;
C_noret_decl(f_885)
static void C_ccall f_885(C_word c,C_word *av) C_noret;
C_noret_decl(f_889)
static void C_ccall f_889(C_word c,C_word *av) C_noret;
C_noret_decl(f_893)
static void C_ccall f_893(C_word c,C_word *av) C_noret;
C_noret_decl(f_897)
static void C_ccall f_897(C_word c,C_word *av) C_noret;
C_noret_decl(f_901)
static void C_ccall f_901(C_word c,C_word *av) C_noret;
C_noret_decl(f_905)
static void C_ccall f_905(C_word c,C_word *av) C_noret;
C_noret_decl(f_913)
static void C_ccall f_913(C_word c,C_word *av) C_noret;
C_noret_decl(f_921)
static void C_ccall f_921(C_word c,C_word *av) C_noret;
C_noret_decl(f_925)
static void C_ccall f_925(C_word c,C_word *av) C_noret;
C_noret_decl(f_929)
static void C_ccall f_929(C_word c,C_word *av) C_noret;
C_noret_decl(f_933)
static void C_ccall f_933(C_word c,C_word *av) C_noret;
C_noret_decl(f_937)
static void C_ccall f_937(C_word c,C_word *av) C_noret;
C_noret_decl(f_940)
static void C_ccall f_940(C_word c,C_word *av) C_noret;
C_noret_decl(f_944)
static void C_ccall f_944(C_word c,C_word *av) C_noret;
C_noret_decl(f_948)
static void C_ccall f_948(C_word c,C_word *av) C_noret;
C_noret_decl(f_952)
static void C_ccall f_952(C_word c,C_word *av) C_noret;
C_noret_decl(f_956)
static void C_ccall f_956(C_word c,C_word *av) C_noret;
C_noret_decl(f_964)
static void C_ccall f_964(C_word c,C_word *av) C_noret;
C_noret_decl(f_967)
static void C_ccall f_967(C_word c,C_word *av) C_noret;
C_noret_decl(f_975)
static void C_ccall f_975(C_word c,C_word *av) C_noret;
C_noret_decl(f_979)
static void C_ccall f_979(C_word c,C_word *av) C_noret;
C_noret_decl(f_983)
static void C_ccall f_983(C_word c,C_word *av) C_noret;
C_noret_decl(f_991)
static void C_ccall f_991(C_word c,C_word *av) C_noret;
C_noret_decl(f_995)
static void C_ccall f_995(C_word c,C_word *av) C_noret;
C_noret_decl(f_999)
static void C_ccall f_999(C_word c,C_word *av) C_noret;
C_noret_decl(C_toplevel)
C_externexport void C_ccall C_toplevel(C_word c,C_word *av) C_noret;

/* k1001 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1003(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1003,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:35: ∘"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[58]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[58]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
av2[3]=C_fast_retrieve(lf[46]);
tp(4,av2);}}

/* k1005 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1007(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1007,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:33: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
av2[3]=C_i_vector_set(C_fast_retrieve(lf[18]),C_fast_retrieve(lf[33]),C_fast_retrieve(lf[23]));
tp(4,av2);}}

/* k1013 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1015(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1015,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:32: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
av2[3]=C_i_vector_ref(C_fast_retrieve(lf[18]),C_fast_retrieve(lf[33]));
tp(4,av2);}}

/* k1021 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1023(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,4)))){
C_save_and_reclaim((void *)f_1023,c,av);}
a=C_alloc(4);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1027,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=C_fast_retrieve(lf[18]);
t4=C_fast_retrieve(lf[80]);
t5=C_i_check_structure_2(C_fast_retrieve(lf[18]),lf[0],C_SCHEME_FALSE);
C_trace(C_text("analysis/helpers/sorted-slices.scm:4: ##sys#block-set!"));
t6=*((C_word*)lf[5]+1);{
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=t6;
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[18]);
av2[3]=C_fix(2);
av2[4]=C_fast_retrieve(lf[80]);
((C_proc)(void*)(*((C_word*)t6+1)))(5,av2);}}

/* k1025 in k1021 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1027(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,4)))){
C_save_and_reclaim((void *)f_1027,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:29: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
av2[4]=C_fast_retrieve(lf[18]);
tp(5,av2);}}

/* k1029 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1031(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,4)))){
C_save_and_reclaim((void *)f_1031,c,av);}
a=C_alloc(4);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1035,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=C_fast_retrieve(lf[18]);
t4=C_fast_retrieve(lf[65]);
t5=C_i_check_structure_2(C_fast_retrieve(lf[18]),lf[0],C_SCHEME_FALSE);
C_trace(C_text("analysis/helpers/sorted-slices.scm:4: ##sys#block-set!"));
t6=*((C_word*)lf[5]+1);{
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=t6;
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[18]);
av2[3]=C_fix(3);
av2[4]=C_fast_retrieve(lf[65]);
((C_proc)(void*)(*((C_word*)t6+1)))(5,av2);}}

/* k1033 in k1029 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1035(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,4)))){
C_save_and_reclaim((void *)f_1035,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:28: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
av2[4]=C_fast_retrieve(lf[18]);
tp(5,av2);}}

/* k1037 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1039(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,4)))){
C_save_and_reclaim((void *)f_1039,c,av);}
a=C_alloc(4);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1043,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=C_fast_retrieve(lf[18]);
t4=C_fast_retrieve(lf[33]);
t5=C_i_check_structure_2(C_fast_retrieve(lf[18]),lf[0],C_SCHEME_FALSE);
C_trace(C_text("analysis/helpers/sorted-slices.scm:4: ##sys#block-set!"));
t6=*((C_word*)lf[5]+1);{
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=t6;
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[18]);
av2[3]=C_fix(1);
av2[4]=C_fast_retrieve(lf[33]);
((C_proc)(void*)(*((C_word*)t6+1)))(5,av2);}}

/* k1041 in k1037 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1043(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,4)))){
C_save_and_reclaim((void *)f_1043,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:27: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
av2[4]=C_fast_retrieve(lf[18]);
tp(5,av2);}}

/* k1045 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1047(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,2)))){
C_save_and_reclaim((void *)f_1047,c,av);}
a=C_alloc(7);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1055,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1059,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:24: next-2"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[82]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[82]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[33]);
tp(3,av2);}}

/* k1053 in k1045 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1055(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,3)))){
C_save_and_reclaim((void *)f_1055,c,av);}
a=C_alloc(5);
t2=C_fast_retrieve(lf[16]);
t3=C_a_i_record4(&a,4,lf[0],C_fix(0),C_fast_retrieve(lf[16]),t1);
C_trace(C_text("analysis/helpers/sorted-slices.scm:24: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t3;
tp(4,av2);}}

/* k1057 in k1045 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1059(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_1059,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:24: scheme#make-vector"));
t2=*((C_word*)lf[66]+1);{
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

/* k1061 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1063(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1063,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:24: slice"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[0]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[0]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=C_fast_retrieve(lf[83]);
av2[3]=t1;
tp(4,av2);}}

/* k1065 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1067(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(36,c,2)))){
C_save_and_reclaim((void *)f_1067,c,av);}
a=C_alloc(36);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1071,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1075,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
t4=C_s_a_i_minus(&a,2,C_fast_retrieve(lf[33]),C_fix(1));
C_trace(C_text("analysis/helpers/sorted-slices.scm:15: n"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[33]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[33]+1);
av2[1]=t3;
av2[2]=t4;
tp(3,av2);}}

/* k1069 in k1065 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1071(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1071,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:14: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k1073 in k1065 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1075(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(17,c,2)))){
C_save_and_reclaim((void *)f_1075,c,av);}
a=C_alloc(17);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1078,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1086,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t4=C_s_a_i_arithmetic_shift(&a,2,C_fast_retrieve(lf[33]),C_fix(-1));
t5=C_s_a_i_bitwise_ior(&a,2,C_fast_retrieve(lf[33]),t4);
C_trace(C_text("analysis/helpers/sorted-slices.scm:16: n"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[33]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[33]+1);
av2[1]=t3;
av2[2]=t5;
tp(3,av2);}}

/* k1076 in k1073 in k1065 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1078(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(29,c,3)))){
C_save_and_reclaim((void *)f_1078,c,av);}
a=C_alloc(29);
t2=C_s_a_i_plus(&a,2,C_fast_retrieve(lf[33]),C_fix(1));
C_trace(C_text("analysis/helpers/sorted-slices.scm:15: ∃"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[35]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[35]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
av2[3]=t2;
tp(4,av2);}}

/* k1084 in k1073 in k1065 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1086(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(15,c,2)))){
C_save_and_reclaim((void *)f_1086,c,av);}
a=C_alloc(15);
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1090,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t1,tmp=(C_word)a,a+=5,tmp);
t3=C_s_a_i_arithmetic_shift(&a,2,C_fast_retrieve(lf[33]),C_fix(-2));
t4=C_s_a_i_bitwise_ior(&a,2,C_fast_retrieve(lf[33]),t3);
C_trace(C_text("analysis/helpers/sorted-slices.scm:17: n"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[33]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[33]+1);
av2[1]=t2;
av2[2]=t4;
tp(3,av2);}}

/* k1088 in k1084 in k1073 in k1065 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1090(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(16,c,2)))){
C_save_and_reclaim((void *)f_1090,c,av);}
a=C_alloc(16);
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_1094,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=t1,tmp=(C_word)a,a+=6,tmp);
t3=C_s_a_i_arithmetic_shift(&a,2,C_fast_retrieve(lf[33]),C_fix(-4));
t4=C_s_a_i_bitwise_ior(&a,2,C_fast_retrieve(lf[33]),t3);
C_trace(C_text("analysis/helpers/sorted-slices.scm:18: n"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[33]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[33]+1);
av2[1]=t2;
av2[2]=t4;
tp(3,av2);}}

/* k1092 in k1088 in k1084 in k1073 in k1065 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1094(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(17,c,2)))){
C_save_and_reclaim((void *)f_1094,c,av);}
a=C_alloc(17);
t2=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_1098,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],a[6]=t1,tmp=(C_word)a,a+=7,tmp);
t3=C_s_a_i_arithmetic_shift(&a,2,C_fast_retrieve(lf[33]),C_fix(-8));
t4=C_s_a_i_bitwise_ior(&a,2,C_fast_retrieve(lf[33]),t3);
C_trace(C_text("analysis/helpers/sorted-slices.scm:19: n"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[33]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[33]+1);
av2[1]=t2;
av2[2]=t4;
tp(3,av2);}}

/* k1096 in k1092 in k1088 in k1084 in k1073 in k1065 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1098(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(18,c,2)))){
C_save_and_reclaim((void *)f_1098,c,av);}
a=C_alloc(18);
t2=(*a=C_CLOSURE_TYPE|7,a[1]=(C_word)f_1102,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],a[6]=((C_word*)t0)[6],a[7]=t1,tmp=(C_word)a,a+=8,tmp);
t3=C_s_a_i_arithmetic_shift(&a,2,C_fast_retrieve(lf[33]),C_fix(-16));
t4=C_s_a_i_bitwise_ior(&a,2,C_fast_retrieve(lf[33]),t3);
C_trace(C_text("analysis/helpers/sorted-slices.scm:20: n"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[33]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[33]+1);
av2[1]=t2;
av2[2]=t4;
tp(3,av2);}}

/* k1100 in k1096 in k1092 in k1088 in k1084 in k1073 in k1065 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1102(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(19,c,2)))){
C_save_and_reclaim((void *)f_1102,c,av);}
a=C_alloc(19);
t2=(*a=C_CLOSURE_TYPE|8,a[1]=(C_word)f_1106,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],a[6]=((C_word*)t0)[6],a[7]=((C_word*)t0)[7],a[8]=t1,tmp=(C_word)a,a+=9,tmp);
t3=C_s_a_i_arithmetic_shift(&a,2,C_fast_retrieve(lf[33]),C_fix(-32));
t4=C_s_a_i_bitwise_ior(&a,2,C_fast_retrieve(lf[33]),t3);
C_trace(C_text("analysis/helpers/sorted-slices.scm:21: n"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[33]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[33]+1);
av2[1]=t2;
av2[2]=t4;
tp(3,av2);}}

/* k1104 in k1100 in k1096 in k1092 in k1088 in k1084 in k1073 in k1065 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1106(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,7)))){
C_save_and_reclaim((void *)f_1106,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:15: g66"));
t2=((C_word*)t0)[2];{
C_word *av2;
if(c >= 8) {
  av2=av;
} else {
  av2=C_alloc(8);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[3];
av2[2]=((C_word*)t0)[4];
av2[3]=((C_word*)t0)[5];
av2[4]=((C_word*)t0)[6];
av2[5]=((C_word*)t0)[7];
av2[6]=((C_word*)t0)[8];
av2[7]=t1;
((C_proc)C_fast_retrieve_proc(t2))(8,av2);}}

/* a1159 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1160(C_word c,C_word *av){
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
if(C_unlikely(!C_demand(C_calculate_demand(5,c,3)))){
C_save_and_reclaim((void *)f_1160,c,av);}
a=C_alloc(5);
t4=C_i_check_port_2(t3,C_fix(2),C_SCHEME_TRUE,lf[84]);
t5=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1167,a[2]=t1,a[3]=t3,a[4]=t2,tmp=(C_word)a,a+=5,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:12: ##sys#write-char-0"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[88]);
C_word *av2=av;
av2[0]=*((C_word*)lf[88]+1);
av2[1]=t5;
av2[2]=C_make_character(35);
av2[3]=t3;
tp(4,av2);}}

/* k1165 in a1159 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1167(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(12,c,2)))){
C_save_and_reclaim((void *)f_1167,c,av);}
a=C_alloc(12);
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_1170,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],tmp=(C_word)a,a+=5,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1181,a[2]=t2,a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1185,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:12: ⊆v⍋"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[30]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[30]+1);
av2[1]=t4;
av2[2]=((C_word*)t0)[4];
tp(3,av2);}}

/* k1168 in k1165 in a1159 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1170(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,2)))){
C_save_and_reclaim((void *)f_1170,c,av);}
a=C_alloc(4);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_1177,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:12: ⊆v⊥xs"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[57]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[57]+1);
av2[1]=t2;
av2[2]=((C_word*)t0)[4];
tp(3,av2);}}

/* k1175 in k1168 in k1165 in a1159 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1177(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,4)))){
C_save_and_reclaim((void *)f_1177,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:12: ##sys#print"));
t2=*((C_word*)lf[85]+1);{
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
av2[3]=C_SCHEME_TRUE;
av2[4]=((C_word*)t0)[3];
((C_proc)(void*)(*((C_word*)t2+1)))(5,av2);}}

/* k1179 in k1165 in a1159 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1181(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,4)))){
C_save_and_reclaim((void *)f_1181,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:12: ##sys#print"));
t2=*((C_word*)lf[85]+1);{
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=t2;
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
av2[3]=C_SCHEME_TRUE;
av2[4]=((C_word*)t0)[3];
((C_proc)(void*)(*((C_word*)t2+1)))(5,av2);}}

/* k1183 in k1165 in a1159 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_1185(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_1185,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:12: ∀"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[86]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[86]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=C_fast_retrieve(lf[87]);
av2[3]=t1;
tp(4,av2);}}

/* k331 */
static void C_ccall f_333(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_333,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_336,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);{
C_word *av2=av;
av2[0]=C_SCHEME_UNDEFINED;
av2[1]=t2;
C_eval_toplevel(2,av2);}}

/* k334 in k331 */
static void C_ccall f_336(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_336,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_339,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);{
C_word *av2=av;
av2[0]=C_SCHEME_UNDEFINED;
av2[1]=t2;
C_extras_toplevel(2,av2);}}

/* k337 in k334 in k331 */
static void C_ccall f_339(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_339,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_342,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);{
C_word *av2=av;
av2[0]=C_SCHEME_UNDEFINED;
av2[1]=t2;
C_data_2dstructures_toplevel(2,av2);}}

/* k340 in k337 in k334 in k331 */
static void C_ccall f_342(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,4)))){
C_save_and_reclaim((void *)f_342,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_345,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:1: chicken.load#load-extension"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[90]);
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[90]+1);
av2[1]=t2;
av2[2]=lf[91];
av2[3]=C_SCHEME_TRUE;
av2[4]=C_SCHEME_FALSE;
tp(5,av2);}}

/* k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_345(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_345,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_348,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);{
C_word *av2=av;
av2[0]=C_SCHEME_UNDEFINED;
av2[1]=t2;
C_srfi_2d4_toplevel(2,av2);}}

/* k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_348(C_word c,C_word *av){
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
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(30,c,5)))){
C_save_and_reclaim((void *)f_348,c,av);}
a=C_alloc(30);
t2=C_mutate((C_word*)lf[0]+1 /* (set! slice ...) */,lf[0]);
t3=C_mutate((C_word*)lf[1]+1 /* (set! make-slice ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_351,a[2]=((C_word)li0),tmp=(C_word)a,a+=3,tmp));
t4=C_mutate((C_word*)lf[2]+1 /* (set! slice? ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_357,a[2]=((C_word)li1),tmp=(C_word)a,a+=3,tmp));
t5=C_mutate((C_word*)lf[3]+1 /* (set! slice-length ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_363,a[2]=((C_word)li2),tmp=(C_word)a,a+=3,tmp));
t6=C_mutate((C_word*)lf[4]+1 /* (set! slice-length-set! ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_372,a[2]=((C_word)li3),tmp=(C_word)a,a+=3,tmp));
t7=C_mutate((C_word*)lf[6]+1 /* (set! slice-sorts ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_381,a[2]=((C_word)li4),tmp=(C_word)a,a+=3,tmp));
t8=C_mutate((C_word*)lf[7]+1 /* (set! slice-sorts-set! ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_390,a[2]=((C_word)li5),tmp=(C_word)a,a+=3,tmp));
t9=C_mutate((C_word*)lf[8]+1 /* (set! slice-vec ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_399,a[2]=((C_word)li6),tmp=(C_word)a,a+=3,tmp));
t10=C_mutate((C_word*)lf[9]+1 /* (set! slice-vec-set! ...) */,(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_408,a[2]=((C_word)li7),tmp=(C_word)a,a+=3,tmp));
t11=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_418,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t12=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1160,a[2]=((C_word)li8),tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:11: chicken.base#set-record-printer!"));
t13=C_fast_retrieve(lf[89]);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t13;
av2[1]=t11;
av2[2]=lf[0];
av2[3]=t12;
((C_proc)(void*)(*((C_word*)t13+1)))(4,av2);}}

/* make-slice in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_351(C_word c,C_word *av){
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
if(C_unlikely(!C_demand(C_calculate_demand(5,c,1)))){
C_save_and_reclaim((void *)f_351,c,av);}
a=C_alloc(5);
t5=t1;{
C_word *av2=av;
av2[0]=t5;
av2[1]=C_a_i_record4(&a,4,lf[0],t2,t3,t4);
((C_proc)(void*)(*((C_word*)t5+1)))(2,av2);}}

/* slice? in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_357(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2=av[2];
C_word t3;
C_word *a;
if(c!=3) C_bad_argc_2(c,3,t0);
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_357,c,av);}
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=C_i_structurep(t2,lf[0]);
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* slice-length in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_363(C_word c,C_word *av){
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
C_save_and_reclaim((void *)f_363,c,av);}
t3=C_i_check_structure_2(t2,lf[0],lf[3]);
t4=t1;{
C_word *av2=av;
av2[0]=t4;
av2[1]=C_i_block_ref(t2,C_fix(1));
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}

/* slice-length-set! in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_372(C_word c,C_word *av){
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
if(C_unlikely(!C_demand(C_calculate_demand(0,c,4)))){
C_save_and_reclaim((void *)f_372,c,av);}
t4=C_i_check_structure_2(t2,lf[0],C_SCHEME_FALSE);
C_trace(C_text("analysis/helpers/sorted-slices.scm:4: ##sys#block-set!"));
t5=*((C_word*)lf[5]+1);{
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=t5;
av2[1]=t1;
av2[2]=t2;
av2[3]=C_fix(1);
av2[4]=t3;
((C_proc)(void*)(*((C_word*)t5+1)))(5,av2);}}

/* slice-sorts in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_381(C_word c,C_word *av){
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
C_save_and_reclaim((void *)f_381,c,av);}
t3=C_i_check_structure_2(t2,lf[0],lf[6]);
t4=t1;{
C_word *av2=av;
av2[0]=t4;
av2[1]=C_i_block_ref(t2,C_fix(2));
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}

/* slice-sorts-set! in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_390(C_word c,C_word *av){
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
if(C_unlikely(!C_demand(C_calculate_demand(0,c,4)))){
C_save_and_reclaim((void *)f_390,c,av);}
t4=C_i_check_structure_2(t2,lf[0],C_SCHEME_FALSE);
C_trace(C_text("analysis/helpers/sorted-slices.scm:4: ##sys#block-set!"));
t5=*((C_word*)lf[5]+1);{
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=t5;
av2[1]=t1;
av2[2]=t2;
av2[3]=C_fix(2);
av2[4]=t3;
((C_proc)(void*)(*((C_word*)t5+1)))(5,av2);}}

/* slice-vec in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_399(C_word c,C_word *av){
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
C_save_and_reclaim((void *)f_399,c,av);}
t3=C_i_check_structure_2(t2,lf[0],lf[8]);
t4=t1;{
C_word *av2=av;
av2[0]=t4;
av2[1]=C_i_block_ref(t2,C_fix(3));
((C_proc)(void*)(*((C_word*)t4+1)))(2,av2);}}

/* slice-vec-set! in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_408(C_word c,C_word *av){
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
if(C_unlikely(!C_demand(C_calculate_demand(0,c,4)))){
C_save_and_reclaim((void *)f_408,c,av);}
t4=C_i_check_structure_2(t2,lf[0],C_SCHEME_FALSE);
C_trace(C_text("analysis/helpers/sorted-slices.scm:4: ##sys#block-set!"));
t5=*((C_word*)lf[5]+1);{
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=t5;
av2[1]=t1;
av2[2]=t2;
av2[3]=C_fix(3);
av2[4]=t3;
((C_proc)(void*)(*((C_word*)t5+1)))(5,av2);}}

/* k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_418(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,2)))){
C_save_and_reclaim((void *)f_418,c,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_421,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1067,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:14: next-2"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[82]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[82]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[33]);
tp(3,av2);}}

/* k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_421(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(9,c,2)))){
C_save_and_reclaim((void *)f_421,c,av);}
a=C_alloc(9);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_424,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1047,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1063,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:24: n"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[33]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[33]+1);
av2[1]=t4;
av2[2]=C_fix(128);
tp(3,av2);}}

/* k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_424(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,3)))){
C_save_and_reclaim((void *)f_424,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_427,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:26: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[81]);
av2[3]=C_fast_retrieve(lf[0]);
tp(4,av2);}}

/* k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_427(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,3)))){
C_save_and_reclaim((void *)f_427,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_430,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:26: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[46]);
av2[3]=C_fast_retrieve(lf[3]);
tp(4,av2);}}

/* k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_430(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,3)))){
C_save_and_reclaim((void *)f_430,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_433,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:26: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[28]);
av2[3]=C_fast_retrieve(lf[8]);
tp(4,av2);}}

/* k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_433(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,3)))){
C_save_and_reclaim((void *)f_433,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_436,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:26: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[30]);
av2[3]=C_fast_retrieve(lf[6]);
tp(4,av2);}}

/* k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_436(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,3)))){
C_save_and_reclaim((void *)f_436,c,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_439,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1039,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:27: ⊆vρ!"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[70]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[70]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[33]);
av2[3]=C_fast_retrieve(lf[18]);
tp(4,av2);}}

/* k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_439(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,3)))){
C_save_and_reclaim((void *)f_439,c,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_442,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1031,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:28: ⊆vv!"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[72]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[72]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[65]);
av2[3]=C_fast_retrieve(lf[18]);
tp(4,av2);}}

/* k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_442(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,3)))){
C_save_and_reclaim((void *)f_442,c,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_445,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1023,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:29: ⊆v⍋!"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[43]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[43]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[80]);
av2[3]=C_fast_retrieve(lf[18]);
tp(4,av2);}}

/* k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_445(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,3)))){
C_save_and_reclaim((void *)f_445,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_448,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:31: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[68]);
av2[3]=*((C_word*)lf[79]+1);
tp(4,av2);}}

/* k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_448(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,3)))){
C_save_and_reclaim((void *)f_448,c,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_451,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1015,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:32: vι"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[37]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[37]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[33]);
av2[3]=C_fast_retrieve(lf[18]);
tp(4,av2);}}

/* k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_451(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,4)))){
C_save_and_reclaim((void *)f_451,c,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_454,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1007,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:33: v!"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[71]);
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[71]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[33]);
av2[3]=C_fast_retrieve(lf[23]);
av2[4]=C_fast_retrieve(lf[18]);
tp(5,av2);}}

/* k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_454(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(9,c,3)))){
C_save_and_reclaim((void *)f_454,c,av);}
a=C_alloc(9);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_457,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_999,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_1003,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:35: D"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[51]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[51]+1);
av2[1]=t4;
av2[2]=*((C_word*)lf[78]+1);
av2[3]=C_fix(0);
tp(4,av2);}}

/* k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_457(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,3)))){
C_save_and_reclaim((void *)f_457,c,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_460,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_979,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:36: slice-ref"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[77]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[77]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[33]);
av2[3]=C_fast_retrieve(lf[18]);
tp(4,av2);}}

/* k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_460(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,3)))){
C_save_and_reclaim((void *)f_460,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_463,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:37: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[76]);
av2[3]=C_fast_retrieve(lf[77]);
tp(4,av2);}}

/* k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 in ... */
static void C_ccall f_463(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,3)))){
C_save_and_reclaim((void *)f_463,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_466,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:37: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[74]);
av2[3]=C_fast_retrieve(lf[75]);
tp(4,av2);}}

/* k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in ... */
static void C_ccall f_466(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,4)))){
C_save_and_reclaim((void *)f_466,c,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_469,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_929,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:39: copy-vector!"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[64]);
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[64]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[23]);
av2[3]=C_fast_retrieve(lf[18]);
av2[4]=C_fast_retrieve(lf[33]);
tp(5,av2);}}

/* k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in ... */
static void C_ccall f_469(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,2)))){
C_save_and_reclaim((void *)f_469,c,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_472,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_855,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:42: grow-slice!"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[69]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[69]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[23]);
tp(3,av2);}}

/* k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in ... */
static void C_ccall f_472(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,3)))){
C_save_and_reclaim((void *)f_472,c,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_475,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_812,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:50: slice-append!"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[54]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[54]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[23]);
av2[3]=C_fast_retrieve(lf[18]);
tp(4,av2);}}

/* k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in ... */
static void C_ccall f_475(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,2)))){
C_save_and_reclaim((void *)f_475,c,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_478,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_761,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:54: copy-slice"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[56]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[56]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[18]);
tp(3,av2);}}

/* k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in ... */
static void C_ccall f_478(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,2)))){
C_save_and_reclaim((void *)f_478,c,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_481,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_745,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:58: slice->vector"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[61]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[61]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[18]);
tp(3,av2);}}

/* k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in ... */
static void C_ccall f_481(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,3)))){
C_save_and_reclaim((void *)f_481,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_484,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:59: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[60]);
av2[3]=C_fast_retrieve(lf[61]);
tp(4,av2);}}

/* k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in ... */
static void C_ccall f_484(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,3)))){
C_save_and_reclaim((void *)f_484,c,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_487,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_741,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:59: ∘"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[58]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[58]+1);
av2[1]=t3;
av2[2]=*((C_word*)lf[59]+1);
av2[3]=C_fast_retrieve(lf[60]);
tp(4,av2);}}

/* k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in ... */
static void C_ccall f_487(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,3)))){
C_save_and_reclaim((void *)f_487,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_490,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:59: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[55]);
av2[3]=C_fast_retrieve(lf[56]);
tp(4,av2);}}

/* k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in ... */
static void C_ccall f_490(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,3)))){
C_save_and_reclaim((void *)f_490,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_493,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:60: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[53]);
av2[3]=C_fast_retrieve(lf[54]);
tp(4,av2);}}

/* k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in ... */
static void C_ccall f_493(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,4)))){
C_save_and_reclaim((void *)f_493,c,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_496,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_713,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:62: slice-ordering"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[45]);
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[45]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[22]);
av2[3]=C_fast_retrieve(lf[18]);
av2[4]=C_fast_retrieve(lf[33]);
tp(5,av2);}}

/* k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in ... */
static void C_ccall f_496(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,3)))){
C_save_and_reclaim((void *)f_496,c,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_499,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_697,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:63: slice-sort"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[44]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[44]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[22]);
av2[3]=C_fast_retrieve(lf[18]);
tp(4,av2);}}

/* k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in ... */
static void C_ccall f_499(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,4)))){
C_save_and_reclaim((void *)f_499,c,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_502,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_662,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:64: slice-sort!"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[41]);
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[41]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[23]);
av2[3]=C_fast_retrieve(lf[22]);
av2[4]=C_fast_retrieve(lf[18]);
tp(5,av2);}}

/* k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in ... */
static void C_ccall f_502(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,3)))){
C_save_and_reclaim((void *)f_502,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_505,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:66: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[40]);
av2[3]=C_fast_retrieve(lf[41]);
tp(4,av2);}}

/* k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in ... */
static void C_ccall f_505(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,5)))){
C_save_and_reclaim((void *)f_505,c,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_508,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_586,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:68: sort-fold"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[26]);
C_word *av2;
if(c >= 6) {
  av2=av;
} else {
  av2=C_alloc(6);
}
av2[0]=*((C_word*)lf[26]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[22]);
av2[3]=C_fast_retrieve(lf[21]);
av2[4]=C_fast_retrieve(lf[18]);
av2[5]=C_fast_retrieve(lf[27]);
tp(6,av2);}}

/* k506 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in ... */
static void C_ccall f_508(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,5)))){
C_save_and_reclaim((void *)f_508,c,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_511,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_554,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:75: sorted-slice-fold"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[15]);
C_word *av2;
if(c >= 6) {
  av2=av;
} else {
  av2=C_alloc(6);
}
av2[0]=*((C_word*)lf[15]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[22]);
av2[3]=C_fast_retrieve(lf[21]);
av2[4]=C_fast_retrieve(lf[17]);
av2[5]=C_fast_retrieve(lf[18]);
tp(6,av2);}}

/* k509 in k506 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in ... */
static void C_ccall f_511(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,4)))){
C_save_and_reclaim((void *)f_511,c,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_514,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_530,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:79: sorted-slice-map"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[13]);
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[13]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[22]);
av2[3]=C_fast_retrieve(lf[17]);
av2[4]=C_fast_retrieve(lf[18]);
tp(5,av2);}}

/* k512 in k509 in k506 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in ... */
static void C_ccall f_514(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,3)))){
C_save_and_reclaim((void *)f_514,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_517,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:81: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[14]);
av2[3]=C_fast_retrieve(lf[15]);
tp(4,av2);}}

/* k515 in k512 in k509 in k506 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in ... */
static void C_ccall f_517(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,3)))){
C_save_and_reclaim((void *)f_517,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_520,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:81: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[12]);
av2[3]=C_fast_retrieve(lf[13]);
tp(4,av2);}}

/* k518 in k515 in k512 in k509 in k506 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in ... */
static void C_ccall f_520(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_520,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_526,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("chicken.base#implicit-exit-handler"));
t3=C_fast_retrieve(lf[10]);{
C_word *av2=av;
av2[0]=t3;
av2[1]=t2;
((C_proc)(void*)(*((C_word*)t3+1)))(2,av2);}}

/* k524 in k518 in k515 in k512 in k509 in k506 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in ... */
static void C_ccall f_526(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,1)))){
C_save_and_reclaim((void *)f_526,c,av);}
t2=t1;{
C_word *av2=av;
av2[0]=t2;
av2[1]=((C_word*)t0)[2];
((C_proc)(void*)(*((C_word*)t2+1)))(2,av2);}}

/* k528 in k509 in k506 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in ... */
static void C_ccall f_530(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(10,c,2)))){
C_save_and_reclaim((void *)f_530,c,av);}
a=C_alloc(10);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_534,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_538,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_542,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:79: α"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[23]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[23]+1);
av2[1]=t4;
av2[2]=C_fast_retrieve(lf[21]);
tp(3,av2);}}

/* k532 in k528 in k509 in k506 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in ... */
static void C_ccall f_534(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_534,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:79: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k536 in k528 in k509 in k506 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in ... */
static void C_ccall f_538(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,5)))){
C_save_and_reclaim((void *)f_538,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:79: sorted-slice-fold"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[15]);
C_word *av2;
if(c >= 6) {
  av2=av;
} else {
  av2=C_alloc(6);
}
av2[0]=*((C_word*)lf[15]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
av2[3]=C_fast_retrieve(lf[16]);
av2[4]=C_fast_retrieve(lf[17]);
av2[5]=C_fast_retrieve(lf[18]);
tp(6,av2);}}

/* k540 in k528 in k509 in k506 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in ... */
static void C_ccall f_542(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,2)))){
C_save_and_reclaim((void *)f_542,c,av);}
a=C_alloc(7);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_546,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_550,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:79: f"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[22]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[22]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[23]);
tp(3,av2);}}

/* k544 in k540 in k528 in k509 in k506 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in ... */
static void C_ccall f_546(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_546,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:79: λ"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[19]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[19]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k548 in k540 in k528 in k509 in k506 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in ... */
static void C_ccall f_550(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_550,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:79: ⊂"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[20]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[20]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
av2[3]=C_fast_retrieve(lf[21]);
tp(4,av2);}}

/* k552 in k506 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in ... */
static void C_ccall f_554(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(13,c,2)))){
C_save_and_reclaim((void *)f_554,c,av);}
a=C_alloc(13);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_558,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_562,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_578,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
t5=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_582,a[2]=t4,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:76: ⊆v⍋"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[30]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[30]+1);
av2[1]=t5;
av2[2]=C_fast_retrieve(lf[18]);
tp(3,av2);}}

/* k556 in k552 in k506 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in ... */
static void C_ccall f_558(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_558,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:75: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k560 in k552 in k506 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in ... */
static void C_ccall f_562(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(10,c,2)))){
C_save_and_reclaim((void *)f_562,c,av);}
a=C_alloc(10);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_566,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_570,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_574,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:77: ⊆vv"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[28]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[28]+1);
av2[1]=t4;
av2[2]=C_fast_retrieve(lf[18]);
tp(3,av2);}}

/* k564 in k560 in k552 in k506 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in ... */
static void C_ccall f_566(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_566,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:76: for"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[24]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[24]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k568 in k560 in k552 in k506 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in ... */
static void C_ccall f_570(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_570,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:77: yield"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[25]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[25]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
tp(3,av2);}}

/* k572 in k560 in k552 in k506 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in ... */
static void C_ccall f_574(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,5)))){
C_save_and_reclaim((void *)f_574,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:77: sort-fold"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[26]);
C_word *av2;
if(c >= 6) {
  av2=av;
} else {
  av2=C_alloc(6);
}
av2[0]=*((C_word*)lf[26]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=C_fast_retrieve(lf[22]);
av2[3]=C_fast_retrieve(lf[21]);
av2[4]=t1;
av2[5]=C_fast_retrieve(lf[27]);
tp(6,av2);}}

/* k576 in k552 in k506 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in ... */
static void C_ccall f_578(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_578,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:76: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=C_fast_retrieve(lf[27]);
av2[3]=t1;
tp(4,av2);}}

/* k580 in k552 in k506 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in ... */
static void C_ccall f_582(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_582,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:76: ∈"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[29]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[29]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=C_fast_retrieve(lf[17]);
av2[3]=t1;
tp(4,av2);}}

/* k584 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in ... */
static void C_ccall f_586(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(13,c,2)))){
C_save_and_reclaim((void *)f_586,c,av);}
a=C_alloc(13);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_590,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_594,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_611,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
t5=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_615,a[2]=t4,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:69: n"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[33]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[33]+1);
av2[1]=t5;
av2[2]=C_fast_retrieve(lf[23]);
tp(3,av2);}}

/* k588 in k584 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in ... */
static void C_ccall f_590(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_590,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:68: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k592 in k584 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in ... */
static void C_ccall f_594(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_594,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_597,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:69: g84"));
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=t2;
((C_proc)C_fast_retrieve_proc(t3))(2,av2);}}

/* k595 in k592 in k584 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in ... */
static void C_ccall f_597(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(9,c,3)))){
C_save_and_reclaim((void *)f_597,c,av);}
a=C_alloc(9);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_601,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=C_i_u64vector_length(C_fast_retrieve(lf[27]));
t4=C_a_i_fixnum_difference(&a,2,t3,C_fix(1));
C_trace(C_text("analysis/helpers/sorted-slices.scm:73: ▽"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[32]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[32]+1);
av2[1]=t2;
av2[2]=t4;
av2[3]=C_fast_retrieve(lf[21]);
tp(4,av2);}}

/* k599 in k595 in k592 in k584 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in ... */
static void C_ccall f_601(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_601,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:69: ∃▽"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[31]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[31]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k609 in k584 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in ... */
static void C_ccall f_611(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_611,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:69: ▽"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[32]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[32]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
tp(3,av2);}}

/* k613 in k584 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in ... */
static void C_ccall f_615(C_word c,C_word *av){
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
if(C_unlikely(!C_demand(C_calculate_demand(14,c,3)))){
C_save_and_reclaim((void *)f_615,c,av);}
a=C_alloc(14);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_619,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=C_i_lessp(C_fast_retrieve(lf[33]),C_fix(0));
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_627,a[2]=t2,a[3]=t3,tmp=(C_word)a,a+=4,tmp);
t5=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_631,a[2]=t4,tmp=(C_word)a,a+=3,tmp);
t6=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_658,a[2]=t5,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:71: srfi-4#u64vector-ref"));
t7=*((C_word*)lf[39]+1);{
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=t7;
av2[1]=t6;
av2[2]=C_fast_retrieve(lf[27]);
av2[3]=C_fast_retrieve(lf[33]);
((C_proc)(void*)(*((C_word*)t7+1)))(4,av2);}}

/* k617 in k613 in k584 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in ... */
static void C_ccall f_619(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_619,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:69: λ"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[19]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[19]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k625 in k613 in k584 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in ... */
static void C_ccall f_627(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,4)))){
C_save_and_reclaim((void *)f_627,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:69: ?"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[34]);
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[34]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=C_fast_retrieve(lf[23]);
av2[4]=t1;
tp(5,av2);}}

/* k629 in k613 in k584 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in ... */
static void C_ccall f_631(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(10,c,3)))){
C_save_and_reclaim((void *)f_631,c,av);}
a=C_alloc(10);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_634,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_650,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_654,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:71: vι"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[37]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[37]+1);
av2[1]=t4;
av2[2]=C_fast_retrieve(lf[38]);
av2[3]=C_fast_retrieve(lf[18]);
tp(4,av2);}}

/* k632 in k629 in k613 in k584 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in ... */
static void C_ccall f_634(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(37,c,3)))){
C_save_and_reclaim((void *)f_634,c,av);}
a=C_alloc(37);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_638,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=C_s_a_i_minus(&a,2,C_fast_retrieve(lf[33]),C_fix(1));
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_646,a[2]=t2,a[3]=t3,tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:72: f"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[22]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[22]+1);
av2[1]=t4;
av2[2]=C_fast_retrieve(lf[36]);
av2[3]=C_fast_retrieve(lf[23]);
tp(4,av2);}}

/* k636 in k632 in k629 in k613 in k584 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in ... */
static void C_ccall f_638(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_638,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:71: ∃"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[35]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[35]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k644 in k632 in k629 in k613 in k584 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in ... */
static void C_ccall f_646(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_646,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:72: ▽"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[32]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[32]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k648 in k629 in k613 in k584 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in ... */
static void C_ccall f_650(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_650,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:71: g86"));
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

/* k652 in k629 in k613 in k584 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in ... */
static void C_ccall f_654(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_654,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:71: x"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[36]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[36]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
tp(3,av2);}}

/* k656 in k613 in k584 in k503 in k500 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in ... */
static void C_ccall f_658(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_658,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:71: ι64"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[38]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[38]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
tp(3,av2);}}

/* k660 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in ... */
static void C_ccall f_662(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(10,c,2)))){
C_save_and_reclaim((void *)f_662,c,av);}
a=C_alloc(10);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_666,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_670,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_693,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:65: ⊆v⍋"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[30]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[30]+1);
av2[1]=t4;
av2[2]=C_fast_retrieve(lf[18]);
tp(3,av2);}}

/* k664 in k660 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in ... */
static void C_ccall f_666(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,4)))){
C_save_and_reclaim((void *)f_666,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:64: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
av2[4]=C_fast_retrieve(lf[18]);
tp(5,av2);}}

/* k668 in k660 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in ... */
static void C_ccall f_670(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_670,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_673,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:65: g79"));
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=t2;
((C_proc)C_fast_retrieve_proc(t3))(2,av2);}}

/* k671 in k668 in k660 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in ... */
static void C_ccall f_673(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,3)))){
C_save_and_reclaim((void *)f_673,c,av);}
a=C_alloc(7);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_677,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_689,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:65: slice-sort"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[44]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[44]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[22]);
av2[3]=C_fast_retrieve(lf[18]);
tp(4,av2);}}

/* k675 in k671 in k668 in k660 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in ... */
static void C_ccall f_677(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_677,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:65: ∃"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[35]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[35]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k687 in k671 in k668 in k660 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in ... */
static void C_ccall f_689(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(9,c,3)))){
C_save_and_reclaim((void *)f_689,c,av);}
a=C_alloc(9);
t2=C_a_i_list(&a,2,C_fast_retrieve(lf[23]),t1);
t3=C_a_i_cons(&a,2,t2,C_fast_retrieve(lf[42]));
C_trace(C_text("analysis/helpers/sorted-slices.scm:65: ⊆v⍋!"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[43]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[43]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t3;
av2[3]=C_fast_retrieve(lf[18]);
tp(4,av2);}}

/* k691 in k660 in k497 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in ... */
static void C_ccall f_693(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_693,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:65: ⍋s"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[42]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[42]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
tp(3,av2);}}

/* k695 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in ... */
static void C_ccall f_697(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,2)))){
C_save_and_reclaim((void *)f_697,c,av);}
a=C_alloc(7);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_701,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_705,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:63: ⊆vv"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[28]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[28]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[18]);
tp(3,av2);}}

/* k699 in k695 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in ... */
static void C_ccall f_701(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_701,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:63: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k703 in k695 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in ... */
static void C_ccall f_705(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,2)))){
C_save_and_reclaim((void *)f_705,c,av);}
a=C_alloc(4);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_709,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:63: ⊆vρ"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[46]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[46]+1);
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[18]);
tp(3,av2);}}

/* k707 in k703 in k695 in k494 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in ... */
static void C_ccall f_709(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,4)))){
C_save_and_reclaim((void *)f_709,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:63: slice-ordering"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[45]);
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[45]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=C_fast_retrieve(lf[22]);
av2[3]=((C_word*)t0)[3];
av2[4]=t1;
tp(5,av2);}}

/* k711 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in ... */
static void C_ccall f_713(C_word c,C_word *av){
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
if(C_unlikely(!C_demand(C_calculate_demand(16,c,2)))){
C_save_and_reclaim((void *)f_713,c,av);}
a=C_alloc(16);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_717,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_721,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_725,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
t5=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_733,a[2]=t4,tmp=(C_word)a,a+=3,tmp);
t6=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_737,a[2]=t5,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:62: ⍨"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[52]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[52]+1);
av2[1]=t6;
av2[2]=C_fast_retrieve(lf[37]);
tp(3,av2);}}

/* k715 in k711 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in ... */
static void C_ccall f_717(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_717,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:62: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k719 in k711 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in ... */
static void C_ccall f_721(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_721,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:62: srfi-4#list->u64vector"));
t2=C_fast_retrieve(lf[47]);{
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

/* k723 in k711 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in ... */
static void C_ccall f_725(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,2)))){
C_save_and_reclaim((void *)f_725,c,av);}
a=C_alloc(4);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_729,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:62: srfi-1#iota"));
t3=C_fast_retrieve(lf[49]);{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t3;
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[33]);
((C_proc)(void*)(*((C_word*)t3+1)))(3,av2);}}

/* k727 in k723 in k711 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in ... */
static void C_ccall f_729(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_729,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:62: ⍋"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[48]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[48]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k731 in k711 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in ... */
static void C_ccall f_733(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_733,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:62: O"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[50]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[50]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=C_fast_retrieve(lf[22]);
av2[3]=t1;
tp(4,av2);}}

/* k735 in k711 in k491 in k488 in k485 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in ... */
static void C_ccall f_737(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_737,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:62: D"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[51]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[51]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
av2[3]=C_fast_retrieve(lf[18]);
tp(4,av2);}}

/* k739 in k482 in k479 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in ... */
static void C_ccall f_741(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_741,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:59: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=C_fast_retrieve(lf[57]);
av2[3]=t1;
tp(4,av2);}}

/* k743 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in ... */
static void C_ccall f_745(C_word c,C_word *av){
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
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,4)))){
C_save_and_reclaim((void *)f_745,c,av);}
a=C_alloc(4);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_749,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=C_fast_retrieve(lf[18]);
t4=C_i_check_structure_2(C_fast_retrieve(lf[18]),lf[0],lf[8]);
t5=C_i_block_ref(C_fast_retrieve(lf[18]),C_fix(3));
t6=C_fast_retrieve(lf[18]);
t7=C_i_check_structure_2(C_fast_retrieve(lf[18]),lf[0],lf[3]);
C_trace(C_text("analysis/helpers/sorted-slices.scm:58: chicken.base#subvector"));
t8=C_fast_retrieve(lf[62]);{
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=t8;
av2[1]=t2;
av2[2]=t5;
av2[3]=C_fix(0);
av2[4]=C_i_block_ref(C_fast_retrieve(lf[18]),C_fix(1));
((C_proc)(void*)(*((C_word*)t8+1)))(5,av2);}}

/* k747 in k743 in k476 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in ... */
static void C_ccall f_749(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_749,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:58: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k759 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in ... */
static void C_ccall f_761(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(10,c,2)))){
C_save_and_reclaim((void *)f_761,c,av);}
a=C_alloc(10);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_765,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_769,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_808,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:55: ⊆vρ"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[46]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[46]+1);
av2[1]=t4;
av2[2]=C_fast_retrieve(lf[18]);
tp(3,av2);}}

/* k763 in k759 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in ... */
static void C_ccall f_765(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_765,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:54: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k767 in k759 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in ... */
static void C_ccall f_769(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(10,c,2)))){
C_save_and_reclaim((void *)f_769,c,av);}
a=C_alloc(10);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_772,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_792,a[2]=t1,a[3]=t2,tmp=(C_word)a,a+=4,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_804,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:55: ⊆vv"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[28]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[28]+1);
av2[1]=t4;
av2[2]=C_fast_retrieve(lf[18]);
tp(3,av2);}}

/* k770 in k767 in k759 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in ... */
static void C_ccall f_772(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,2)))){
C_save_and_reclaim((void *)f_772,c,av);}
a=C_alloc(4);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_780,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:56: ⊆v⍋"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[30]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[30]+1);
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[18]);
tp(3,av2);}}

/* k778 in k770 in k767 in k759 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in ... */
static void C_ccall f_780(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(8,c,2)))){
C_save_and_reclaim((void *)f_780,c,av);}
a=C_alloc(8);
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_784,a[2]=t1,a[3]=((C_word*)t0)[2],a[4]=((C_word*)t0)[3],tmp=(C_word)a,a+=5,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_788,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:56: scheme#make-vector"));
t4=*((C_word*)lf[66]+1);{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t4;
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[67]);
((C_proc)(void*)(*((C_word*)t4+1)))(3,av2);}}

/* k782 in k778 in k770 in k767 in k759 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in ... */
static void C_ccall f_784(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,3)))){
C_save_and_reclaim((void *)f_784,c,av);}
a=C_alloc(5);
t2=C_fast_retrieve(lf[63]);
t3=C_a_i_record4(&a,4,lf[0],C_fast_retrieve(lf[63]),((C_word*)t0)[2],t1);
C_trace(C_text("analysis/helpers/sorted-slices.scm:55: ∃"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[35]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[35]+1);
av2[1]=((C_word*)t0)[3];
av2[2]=((C_word*)t0)[4];
av2[3]=t3;
tp(4,av2);}}

/* k786 in k778 in k770 in k767 in k759 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in ... */
static void C_ccall f_788(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,4)))){
C_save_and_reclaim((void *)f_788,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:56: copy-vector!"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[64]);
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[64]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=C_fast_retrieve(lf[65]);
av2[3]=t1;
av2[4]=C_fast_retrieve(lf[63]);
tp(5,av2);}}

/* k790 in k767 in k759 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in ... */
static void C_ccall f_792(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(8,c,2)))){
C_save_and_reclaim((void *)f_792,c,av);}
a=C_alloc(8);
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_796,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t1,tmp=(C_word)a,a+=5,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_800,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:55: vρ"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[68]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[68]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[65]);
tp(3,av2);}}

/* k794 in k790 in k767 in k759 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in ... */
static void C_ccall f_796(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_796,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:55: g77"));
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

/* k798 in k790 in k767 in k759 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in ... */
static void C_ccall f_800(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_800,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:55: vl"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[67]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[67]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
tp(3,av2);}}

/* k802 in k767 in k759 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in ... */
static void C_ccall f_804(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_804,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:55: v"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[65]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[65]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
tp(3,av2);}}

/* k806 in k759 in k473 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in ... */
static void C_ccall f_808(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_808,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:55: l"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[63]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[63]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
tp(3,av2);}}

/* k810 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in ... */
static void C_ccall f_812(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(10,c,2)))){
C_save_and_reclaim((void *)f_812,c,av);}
a=C_alloc(10);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_816,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_820,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_851,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:51: ⊆vρ"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[46]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[46]+1);
av2[1]=t4;
av2[2]=C_fast_retrieve(lf[18]);
tp(3,av2);}}

/* k814 in k810 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in ... */
static void C_ccall f_816(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_816,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:50: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k818 in k810 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in ... */
static void C_ccall f_820(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_820,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_823,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:51: g75"));
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=t2;
((C_proc)C_fast_retrieve_proc(t3))(2,av2);}}

/* k821 in k818 in k810 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in ... */
static void C_ccall f_823(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,2)))){
C_save_and_reclaim((void *)f_823,c,av);}
a=C_alloc(7);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_827,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_847,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:52: ⊆vv"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[28]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[28]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[18]);
tp(3,av2);}}

/* k825 in k821 in k818 in k810 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in ... */
static void C_ccall f_827(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(34,c,3)))){
C_save_and_reclaim((void *)f_827,c,av);}
a=C_alloc(34);
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_831,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t1,tmp=(C_word)a,a+=5,tmp);
t3=C_s_a_i_plus(&a,2,C_fast_retrieve(lf[63]),C_fix(1));
C_trace(C_text("analysis/helpers/sorted-slices.scm:52: ⊆vρ!"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[70]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[70]+1);
av2[1]=t2;
av2[2]=t3;
av2[3]=C_fast_retrieve(lf[18]);
tp(4,av2);}}

/* k829 in k825 in k821 in k818 in k810 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in ... */
static void C_ccall f_831(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(6,c,3)))){
C_save_and_reclaim((void *)f_831,c,av);}
a=C_alloc(6);
t2=(*a=C_CLOSURE_TYPE|5,a[1]=(C_word)f_835,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=t1,tmp=(C_word)a,a+=6,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:52: ⊆v⍋!"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[43]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[43]+1);
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[16]);
av2[3]=C_fast_retrieve(lf[18]);
tp(4,av2);}}

/* k833 in k829 in k825 in k821 in k818 in k810 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in ... */
static void C_ccall f_835(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,2)))){
C_save_and_reclaim((void *)f_835,c,av);}
a=C_alloc(7);
t2=(*a=C_CLOSURE_TYPE|6,a[1]=(C_word)f_839,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=((C_word*)t0)[4],a[5]=((C_word*)t0)[5],a[6]=t1,tmp=(C_word)a,a+=7,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:52: grow-slice!"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[69]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[69]+1);
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[18]);
tp(3,av2);}}

/* k837 in k833 in k829 in k825 in k821 in k818 in k810 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in ... */
static void C_ccall f_839(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,6)))){
C_save_and_reclaim((void *)f_839,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:51: ∃"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[35]);
C_word *av2;
if(c >= 7) {
  av2=av;
} else {
  av2=C_alloc(7);
}
av2[0]=*((C_word*)lf[35]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=((C_word*)t0)[4];
av2[4]=((C_word*)t0)[5];
av2[5]=((C_word*)t0)[6];
av2[6]=t1;
tp(7,av2);}}

/* k845 in k821 in k818 in k810 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in ... */
static void C_ccall f_847(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,4)))){
C_save_and_reclaim((void *)f_847,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:52: v!"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[71]);
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[71]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=C_fast_retrieve(lf[63]);
av2[3]=C_fast_retrieve(lf[23]);
av2[4]=t1;
tp(5,av2);}}

/* k849 in k810 in k470 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in ... */
static void C_ccall f_851(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_851,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:51: l"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[63]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[63]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
tp(3,av2);}}

/* k853 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in ... */
static void C_ccall f_855(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(13,c,2)))){
C_save_and_reclaim((void *)f_855,c,av);}
a=C_alloc(13);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_859,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_863,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_921,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
t5=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_925,a[2]=t4,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:43: ⊆vv"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[28]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[28]+1);
av2[1]=t5;
av2[2]=C_fast_retrieve(lf[23]);
tp(3,av2);}}

/* k857 in k853 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in ... */
static void C_ccall f_859(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_859,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:42: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k861 in k853 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in ... */
static void C_ccall f_863(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_863,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_866,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:43: g71"));
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=t2;
((C_proc)C_fast_retrieve_proc(t3))(2,av2);}}

/* k864 in k861 in k853 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in ... */
static void C_ccall f_866(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,2)))){
C_save_and_reclaim((void *)f_866,c,av);}
a=C_alloc(7);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_870,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_913,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:44: ⊆vρ"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[46]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[46]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[23]);
tp(3,av2);}}

/* k868 in k864 in k861 in k853 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in ... */
static void C_ccall f_870(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_870,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:43: ∃"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[35]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[35]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k876 in k911 in k864 in k861 in k853 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in ... */
static void C_ccall f_878(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,4)))){
C_save_and_reclaim((void *)f_878,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:44: ?"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[34]);
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[34]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
av2[4]=C_fast_retrieve(lf[23]);
tp(5,av2);}}

/* k880 in k911 in k864 in k861 in k853 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in ... */
static void C_ccall f_882(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_882,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_885,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:45: g73"));
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=t2;
((C_proc)C_fast_retrieve_proc(t3))(2,av2);}}

/* k883 in k880 in k911 in k864 in k861 in k853 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in ... */
static void C_ccall f_885(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,2)))){
C_save_and_reclaim((void *)f_885,c,av);}
a=C_alloc(7);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_889,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_897,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:46: ⊆vv"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[28]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[28]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[23]);
tp(3,av2);}}

/* k887 in k883 in k880 in k911 in k864 in k861 in k853 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in ... */
static void C_ccall f_889(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(5,c,3)))){
C_save_and_reclaim((void *)f_889,c,av);}
a=C_alloc(5);
t2=(*a=C_CLOSURE_TYPE|4,a[1]=(C_word)f_893,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],a[4]=t1,tmp=(C_word)a,a+=5,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:47: ⊆vv!"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[72]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[72]+1);
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[18]);
av2[3]=C_fast_retrieve(lf[23]);
tp(4,av2);}}

/* k891 in k887 in k883 in k880 in k911 in k864 in k861 in k853 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in ... */
static void C_ccall f_893(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,4)))){
C_save_and_reclaim((void *)f_893,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:45: ∃"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[35]);
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[35]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=((C_word*)t0)[4];
av2[4]=t1;
tp(5,av2);}}

/* k895 in k883 in k880 in k911 in k864 in k861 in k853 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in ... */
static void C_ccall f_897(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,2)))){
C_save_and_reclaim((void *)f_897,c,av);}
a=C_alloc(4);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_901,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:46: ⊆vρ"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[46]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[46]+1);
av2[1]=t2;
av2[2]=C_fast_retrieve(lf[23]);
tp(3,av2);}}

/* k899 in k895 in k883 in k880 in k911 in k864 in k861 in k853 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in ... */
static void C_ccall f_901(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,4)))){
C_save_and_reclaim((void *)f_901,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:46: copy-vector!"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[64]);
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[64]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=C_fast_retrieve(lf[18]);
av2[4]=t1;
tp(5,av2);}}

/* k903 in k911 in k864 in k861 in k853 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in ... */
static void C_ccall f_905(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_905,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:45: ω"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[18]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[18]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
tp(3,av2);}}

/* k911 in k864 in k861 in k853 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in ... */
static void C_ccall f_913(C_word c,C_word *av){
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
if(C_unlikely(!C_demand(C_calculate_demand(10,c,2)))){
C_save_and_reclaim((void *)f_913,c,av);}
a=C_alloc(10);
t2=C_fixnum_divide(C_fast_retrieve(lf[63]),C_fix(2));
t3=C_i_greaterp(t1,t2);
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_878,a[2]=((C_word*)t0)[2],a[3]=t3,tmp=(C_word)a,a+=4,tmp);
t5=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_882,a[2]=t4,tmp=(C_word)a,a+=3,tmp);
t6=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_905,a[2]=t5,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:45: scheme#make-vector"));
t7=*((C_word*)lf[66]+1);{
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=t7;
av2[1]=t6;
av2[2]=C_fixnum_times(C_fix(2),C_fast_retrieve(lf[63]));
((C_proc)(void*)(*((C_word*)t7+1)))(3,av2);}}

/* k919 in k853 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in ... */
static void C_ccall f_921(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_921,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:43: l"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[63]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[63]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
tp(3,av2);}}

/* k923 in k853 in k467 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in ... */
static void C_ccall f_925(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_925,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:43: vρ"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[68]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[68]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
tp(3,av2);}}

/* k927 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in ... */
static void C_ccall f_929(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(13,c,2)))){
C_save_and_reclaim((void *)f_929,c,av);}
a=C_alloc(13);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_933,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_937,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
t4=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_948,a[2]=t3,tmp=(C_word)a,a+=3,tmp);
t5=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_952,a[2]=t4,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:40: m"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[73]);
C_word *av2=av;
av2[0]=*((C_word*)lf[73]+1);
av2[1]=t5;
tp(2,av2);}}

/* k931 in k927 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in ... */
static void C_ccall f_933(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_933,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:39: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k935 in k927 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in ... */
static void C_ccall f_937(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(3,c,2)))){
C_save_and_reclaim((void *)f_937,c,av);}
a=C_alloc(3);
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_940,a[2]=((C_word*)t0)[2],tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:40: g68"));
t3=t1;{
C_word *av2=av;
av2[0]=t3;
av2[1]=t2;
((C_proc)C_fast_retrieve_proc(t3))(2,av2);}}

/* k938 in k935 in k927 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in ... */
static void C_ccall f_940(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(4,c,2)))){
C_save_and_reclaim((void *)f_940,c,av);}
a=C_alloc(4);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_944,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:40: ▽"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[32]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[32]+1);
av2[1]=t2;
av2[2]=C_fix(0);
tp(3,av2);}}

/* k942 in k938 in k935 in k927 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in ... */
static void C_ccall f_944(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_944,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:40: ∃▽"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[31]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[31]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k946 in k927 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in ... */
static void C_ccall f_948(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,2)))){
C_save_and_reclaim((void *)f_948,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:40: ▽"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[32]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[32]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=t1;
tp(3,av2);}}

/* k950 in k927 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in ... */
static void C_ccall f_952(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word t5;
C_word t6;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(11,c,3)))){
C_save_and_reclaim((void *)f_952,c,av);}
a=C_alloc(11);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_956,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=C_i_nequalp(C_fast_retrieve(lf[73]),C_fast_retrieve(lf[33]));
t4=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_964,a[2]=t2,a[3]=t3,tmp=(C_word)a,a+=4,tmp);
t5=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_975,a[2]=t4,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:40: vι"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[37]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[37]+1);
av2[1]=t5;
av2[2]=C_fast_retrieve(lf[73]);
av2[3]=C_fast_retrieve(lf[23]);
tp(4,av2);}}

/* k954 in k950 in k927 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in ... */
static void C_ccall f_956(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_956,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:40: λ"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[19]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[19]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k962 in k950 in k927 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in ... */
static void C_ccall f_964(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(33,c,2)))){
C_save_and_reclaim((void *)f_964,c,av);}
a=C_alloc(33);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_967,a[2]=((C_word*)t0)[2],a[3]=((C_word*)t0)[3],tmp=(C_word)a,a+=4,tmp);
t3=C_s_a_i_plus(&a,2,C_fast_retrieve(lf[73]),C_fix(1));
C_trace(C_text("analysis/helpers/sorted-slices.scm:40: ▽"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[32]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[32]+1);
av2[1]=t2;
av2[2]=t3;
tp(3,av2);}}

/* k965 in k962 in k950 in k927 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in ... */
static void C_ccall f_967(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,4)))){
C_save_and_reclaim((void *)f_967,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:40: ?"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[34]);
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[34]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=C_fast_retrieve(lf[18]);
av2[4]=t1;
tp(5,av2);}}

/* k973 in k950 in k927 in k464 in k461 in k458 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in ... */
static void C_ccall f_975(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,4)))){
C_save_and_reclaim((void *)f_975,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:40: v!"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[71]);
C_word *av2;
if(c >= 5) {
  av2=av;
} else {
  av2=C_alloc(5);
}
av2[0]=*((C_word*)lf[71]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=C_fast_retrieve(lf[73]);
av2[3]=t1;
av2[4]=C_fast_retrieve(lf[18]);
tp(5,av2);}}

/* k977 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_979(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(7,c,2)))){
C_save_and_reclaim((void *)f_979,c,av);}
a=C_alloc(7);
t2=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_983,a[2]=((C_word*)t0)[2],a[3]=t1,tmp=(C_word)a,a+=4,tmp);
t3=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_995,a[2]=t2,tmp=(C_word)a,a+=3,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:36: ⊆vρ"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[46]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[46]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[18]);
tp(3,av2);}}

/* k981 in k977 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 in ... */
static void C_ccall f_983(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_983,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:36: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k989 in k993 in k977 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in ... */
static void C_ccall f_991(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_991,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:36: vι"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[37]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[37]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=((C_word*)t0)[3];
av2[3]=t1;
tp(4,av2);}}

/* k993 in k977 in k455 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 in ... */
static void C_ccall f_995(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word t3;
C_word t4;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(9,c,2)))){
C_save_and_reclaim((void *)f_995,c,av);}
a=C_alloc(9);
t2=C_s_a_i_modulo(&a,2,C_fast_retrieve(lf[33]),t1);
t3=(*a=C_CLOSURE_TYPE|3,a[1]=(C_word)f_991,a[2]=((C_word*)t0)[2],a[3]=t2,tmp=(C_word)a,a+=4,tmp);
C_trace(C_text("analysis/helpers/sorted-slices.scm:36: ⊆vv"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[28]);
C_word *av2;
if(c >= 3) {
  av2=av;
} else {
  av2=C_alloc(3);
}
av2[0]=*((C_word*)lf[28]+1);
av2[1]=t3;
av2[2]=C_fast_retrieve(lf[18]);
tp(3,av2);}}

/* k997 in k452 in k449 in k446 in k443 in k440 in k437 in k434 in k431 in k428 in k425 in k422 in k419 in k416 in k346 in k343 in k340 in k337 in k334 in k331 */
static void C_ccall f_999(C_word c,C_word *av){
C_word tmp;
C_word t0=av[0];
C_word t1=av[1];
C_word t2;
C_word *a;
C_check_for_interrupt;
if(C_unlikely(!C_demand(C_calculate_demand(0,c,3)))){
C_save_and_reclaim((void *)f_999,c,av);}
C_trace(C_text("analysis/helpers/sorted-slices.scm:35: ←"));
{C_proc tp=(C_proc)C_fast_retrieve_symbol_proc(lf[11]);
C_word *av2;
if(c >= 4) {
  av2=av;
} else {
  av2=C_alloc(4);
}
av2[0]=*((C_word*)lf[11]+1);
av2[1]=((C_word*)t0)[2];
av2[2]=C_fast_retrieve(lf[75]);
av2[3]=t1;
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
if(C_unlikely(!C_demand_2(644))){
C_save(t1);
C_rereclaim2(644*sizeof(C_word),1);
t1=C_restore;}
a=C_alloc(3);
C_initialize_lf(lf,92);
lf[0]=C_h_intern(&lf[0],5, C_text("slice"));
lf[1]=C_h_intern(&lf[1],10, C_text("make-slice"));
lf[2]=C_h_intern(&lf[2],6, C_text("slice\077"));
lf[3]=C_h_intern(&lf[3],12, C_text("slice-length"));
lf[4]=C_h_intern(&lf[4],17, C_text("slice-length-set!"));
lf[5]=C_h_intern(&lf[5],16, C_text("##sys#block-set!"));
lf[6]=C_h_intern(&lf[6],11, C_text("slice-sorts"));
lf[7]=C_h_intern(&lf[7],16, C_text("slice-sorts-set!"));
lf[8]=C_h_intern(&lf[8],9, C_text("slice-vec"));
lf[9]=C_h_intern(&lf[9],14, C_text("slice-vec-set!"));
lf[10]=C_h_intern(&lf[10],34, C_text("chicken.base#implicit-exit-handler"));
lf[11]=C_h_intern(&lf[11],3, C_text("\342\206\220"));
lf[12]=C_h_intern(&lf[12],10, C_text("\342\212\206v\342\215\213\342\210\200"));
lf[13]=C_h_intern(&lf[13],16, C_text("sorted-slice-map"));
lf[14]=C_h_intern(&lf[14],10, C_text("\342\212\206v\342\215\213\342\207\222"));
lf[15]=C_h_intern(&lf[15],17, C_text("sorted-slice-fold"));
lf[16]=C_h_intern(&lf[16],3, C_text("\342\210\205"));
lf[17]=C_h_intern(&lf[17],1, C_text("k"));
lf[18]=C_h_intern(&lf[18],2, C_text("\317\211"));
lf[19]=C_h_intern(&lf[19],2, C_text("\316\273"));
lf[20]=C_h_intern(&lf[20],3, C_text("\342\212\202"));
lf[21]=C_h_intern(&lf[21],3, C_text("acc"));
lf[22]=C_h_intern(&lf[22],1, C_text("f"));
lf[23]=C_h_intern(&lf[23],2, C_text("\316\261"));
lf[24]=C_h_intern(&lf[24],3, C_text("for"));
lf[25]=C_h_intern(&lf[25],5, C_text("yield"));
lf[26]=C_h_intern(&lf[26],9, C_text("sort-fold"));
lf[27]=C_h_intern(&lf[27],5, C_text("\342\215\213\317\211"));
lf[28]=C_h_intern(&lf[28],5, C_text("\342\212\206vv"));
lf[29]=C_h_intern(&lf[29],3, C_text("\342\210\210"));
lf[30]=C_h_intern(&lf[30],7, C_text("\342\212\206v\342\215\213"));
lf[31]=C_h_intern(&lf[31],6, C_text("\342\210\203\342\226\275"));
lf[32]=C_h_intern(&lf[32],3, C_text("\342\226\275"));
lf[33]=C_h_intern(&lf[33],1, C_text("n"));
lf[34]=C_h_intern(&lf[34],1, C_text("\077"));
lf[35]=C_h_intern(&lf[35],3, C_text("\342\210\203"));
lf[36]=C_h_intern(&lf[36],1, C_text("x"));
lf[37]=C_h_intern(&lf[37],3, C_text("v\316\271"));
lf[38]=C_h_intern(&lf[38],4, C_text("\316\27164"));
lf[39]=C_h_intern(&lf[39],20, C_text("srfi-4#u64vector-ref"));
lf[40]=C_h_intern(&lf[40],8, C_text("\342\215\213\342\212\206v!"));
lf[41]=C_h_intern(&lf[41],11, C_text("slice-sort!"));
lf[42]=C_h_intern(&lf[42],4, C_text("\342\215\213s"));
lf[43]=C_h_intern(&lf[43],8, C_text("\342\212\206v\342\215\213!"));
lf[44]=C_h_intern(&lf[44],10, C_text("slice-sort"));
lf[45]=C_h_intern(&lf[45],14, C_text("slice-ordering"));
lf[46]=C_h_intern(&lf[46],6, C_text("\342\212\206v\317\201"));
lf[47]=C_h_intern(&lf[47],22, C_text("srfi-4#list->u64vector"));
lf[48]=C_h_intern(&lf[48],3, C_text("\342\215\213"));
lf[49]=C_h_intern(&lf[49],11, C_text("srfi-1#iota"));
lf[50]=C_h_intern(&lf[50],1, C_text("O"));
lf[51]=C_h_intern(&lf[51],1, C_text("D"));
lf[52]=C_h_intern(&lf[52],3, C_text("\342\215\250"));
lf[53]=C_h_intern(&lf[53],7, C_text("\342\212\206v\342\212\202"));
lf[54]=C_h_intern(&lf[54],13, C_text("slice-append!"));
lf[55]=C_h_intern(&lf[55],11, C_text("\342\212\206v\342\212\245\342\212\206v"));
lf[56]=C_h_intern(&lf[56],10, C_text("copy-slice"));
lf[57]=C_h_intern(&lf[57],9, C_text("\342\212\206v\342\212\245xs"));
lf[58]=C_h_intern(&lf[58],3, C_text("\342\210\230"));
lf[59]=C_h_intern(&lf[59],19, C_text("scheme#vector->list"));
lf[60]=C_h_intern(&lf[60],8, C_text("\342\212\206v\342\212\245v"));
lf[61]=C_h_intern(&lf[61],13, C_text("slice->vector"));
lf[62]=C_h_intern(&lf[62],22, C_text("chicken.base#subvector"));
lf[63]=C_h_intern(&lf[63],1, C_text("l"));
lf[64]=C_h_intern(&lf[64],12, C_text("copy-vector!"));
lf[65]=C_h_intern(&lf[65],1, C_text("v"));
lf[66]=C_h_intern(&lf[66],18, C_text("scheme#make-vector"));
lf[67]=C_h_intern(&lf[67],2, C_text("vl"));
lf[68]=C_h_intern(&lf[68],3, C_text("v\317\201"));
lf[69]=C_h_intern(&lf[69],11, C_text("grow-slice!"));
lf[70]=C_h_intern(&lf[70],7, C_text("\342\212\206v\317\201!"));
lf[71]=C_h_intern(&lf[71],2, C_text("v!"));
lf[72]=C_h_intern(&lf[72],6, C_text("\342\212\206vv!"));
lf[73]=C_h_intern(&lf[73],1, C_text("m"));
lf[74]=C_h_intern(&lf[74],8, C_text("\342\212\206v\342\210\205\077"));
lf[75]=C_h_intern(&lf[75],11, C_text("slice-null\077"));
lf[76]=C_h_intern(&lf[76],6, C_text("\342\212\206v\316\271"));
lf[77]=C_h_intern(&lf[77],9, C_text("slice-ref"));
lf[78]=C_h_intern(&lf[78],8, C_text("scheme#="));
lf[79]=C_h_intern(&lf[79],20, C_text("scheme#vector-length"));
lf[80]=C_h_intern(&lf[80],1, C_text("s"));
lf[81]=C_h_intern(&lf[81],4, C_text("\342\212\206v"));
lf[82]=C_h_intern(&lf[82],6, C_text("next-2"));
lf[83]=C_h_intern(&lf[83],10, C_text("#!optional"));
lf[84]=C_h_intern(&lf[84],7, C_text("fprintf"));
lf[85]=C_h_intern(&lf[85],11, C_text("##sys#print"));
lf[86]=C_h_intern(&lf[86],3, C_text("\342\210\200"));
lf[87]=C_h_intern(&lf[87],3, C_text("\342\206\221"));
lf[88]=C_h_intern(&lf[88],18, C_text("##sys#write-char-0"));
lf[89]=C_h_intern(&lf[89],32, C_text("chicken.base#set-record-printer!"));
lf[90]=C_h_intern(&lf[90],27, C_text("chicken.load#load-extension"));
lf[91]=C_h_intern(&lf[91],6, C_text("srfi-1"));
C_register_lf2(lf,92,create_ptable());{}
t2=(*a=C_CLOSURE_TYPE|2,a[1]=(C_word)f_333,a[2]=t1,tmp=(C_word)a,a+=3,tmp);{
C_word *av2=av;
av2[0]=C_SCHEME_UNDEFINED;
av2[1]=t2;
C_library_toplevel(2,av2);}}

#ifdef C_ENABLE_PTABLES
static C_PTABLE_ENTRY ptable[186] = {
{C_text("f_1003:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1003},
{C_text("f_1007:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1007},
{C_text("f_1015:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1015},
{C_text("f_1023:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1023},
{C_text("f_1027:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1027},
{C_text("f_1031:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1031},
{C_text("f_1035:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1035},
{C_text("f_1039:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1039},
{C_text("f_1043:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1043},
{C_text("f_1047:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1047},
{C_text("f_1055:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1055},
{C_text("f_1059:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1059},
{C_text("f_1063:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1063},
{C_text("f_1067:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1067},
{C_text("f_1071:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1071},
{C_text("f_1075:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1075},
{C_text("f_1078:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1078},
{C_text("f_1086:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1086},
{C_text("f_1090:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1090},
{C_text("f_1094:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1094},
{C_text("f_1098:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1098},
{C_text("f_1102:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1102},
{C_text("f_1106:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1106},
{C_text("f_1160:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1160},
{C_text("f_1167:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1167},
{C_text("f_1170:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1170},
{C_text("f_1177:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1177},
{C_text("f_1181:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1181},
{C_text("f_1185:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_1185},
{C_text("f_333:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_333},
{C_text("f_336:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_336},
{C_text("f_339:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_339},
{C_text("f_342:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_342},
{C_text("f_345:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_345},
{C_text("f_348:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_348},
{C_text("f_351:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_351},
{C_text("f_357:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_357},
{C_text("f_363:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_363},
{C_text("f_372:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_372},
{C_text("f_381:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_381},
{C_text("f_390:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_390},
{C_text("f_399:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_399},
{C_text("f_408:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_408},
{C_text("f_418:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_418},
{C_text("f_421:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_421},
{C_text("f_424:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_424},
{C_text("f_427:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_427},
{C_text("f_430:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_430},
{C_text("f_433:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_433},
{C_text("f_436:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_436},
{C_text("f_439:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_439},
{C_text("f_442:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_442},
{C_text("f_445:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_445},
{C_text("f_448:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_448},
{C_text("f_451:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_451},
{C_text("f_454:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_454},
{C_text("f_457:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_457},
{C_text("f_460:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_460},
{C_text("f_463:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_463},
{C_text("f_466:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_466},
{C_text("f_469:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_469},
{C_text("f_472:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_472},
{C_text("f_475:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_475},
{C_text("f_478:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_478},
{C_text("f_481:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_481},
{C_text("f_484:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_484},
{C_text("f_487:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_487},
{C_text("f_490:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_490},
{C_text("f_493:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_493},
{C_text("f_496:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_496},
{C_text("f_499:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_499},
{C_text("f_502:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_502},
{C_text("f_505:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_505},
{C_text("f_508:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_508},
{C_text("f_511:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_511},
{C_text("f_514:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_514},
{C_text("f_517:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_517},
{C_text("f_520:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_520},
{C_text("f_526:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_526},
{C_text("f_530:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_530},
{C_text("f_534:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_534},
{C_text("f_538:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_538},
{C_text("f_542:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_542},
{C_text("f_546:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_546},
{C_text("f_550:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_550},
{C_text("f_554:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_554},
{C_text("f_558:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_558},
{C_text("f_562:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_562},
{C_text("f_566:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_566},
{C_text("f_570:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_570},
{C_text("f_574:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_574},
{C_text("f_578:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_578},
{C_text("f_582:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_582},
{C_text("f_586:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_586},
{C_text("f_590:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_590},
{C_text("f_594:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_594},
{C_text("f_597:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_597},
{C_text("f_601:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_601},
{C_text("f_611:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_611},
{C_text("f_615:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_615},
{C_text("f_619:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_619},
{C_text("f_627:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_627},
{C_text("f_631:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_631},
{C_text("f_634:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_634},
{C_text("f_638:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_638},
{C_text("f_646:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_646},
{C_text("f_650:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_650},
{C_text("f_654:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_654},
{C_text("f_658:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_658},
{C_text("f_662:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_662},
{C_text("f_666:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_666},
{C_text("f_670:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_670},
{C_text("f_673:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_673},
{C_text("f_677:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_677},
{C_text("f_689:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_689},
{C_text("f_693:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_693},
{C_text("f_697:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_697},
{C_text("f_701:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_701},
{C_text("f_705:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_705},
{C_text("f_709:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_709},
{C_text("f_713:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_713},
{C_text("f_717:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_717},
{C_text("f_721:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_721},
{C_text("f_725:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_725},
{C_text("f_729:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_729},
{C_text("f_733:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_733},
{C_text("f_737:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_737},
{C_text("f_741:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_741},
{C_text("f_745:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_745},
{C_text("f_749:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_749},
{C_text("f_761:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_761},
{C_text("f_765:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_765},
{C_text("f_769:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_769},
{C_text("f_772:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_772},
{C_text("f_780:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_780},
{C_text("f_784:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_784},
{C_text("f_788:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_788},
{C_text("f_792:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_792},
{C_text("f_796:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_796},
{C_text("f_800:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_800},
{C_text("f_804:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_804},
{C_text("f_808:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_808},
{C_text("f_812:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_812},
{C_text("f_816:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_816},
{C_text("f_820:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_820},
{C_text("f_823:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_823},
{C_text("f_827:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_827},
{C_text("f_831:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_831},
{C_text("f_835:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_835},
{C_text("f_839:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_839},
{C_text("f_847:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_847},
{C_text("f_851:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_851},
{C_text("f_855:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_855},
{C_text("f_859:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_859},
{C_text("f_863:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_863},
{C_text("f_866:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_866},
{C_text("f_870:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_870},
{C_text("f_878:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_878},
{C_text("f_882:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_882},
{C_text("f_885:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_885},
{C_text("f_889:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_889},
{C_text("f_893:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_893},
{C_text("f_897:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_897},
{C_text("f_901:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_901},
{C_text("f_905:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_905},
{C_text("f_913:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_913},
{C_text("f_921:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_921},
{C_text("f_925:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_925},
{C_text("f_929:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_929},
{C_text("f_933:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_933},
{C_text("f_937:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_937},
{C_text("f_940:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_940},
{C_text("f_944:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_944},
{C_text("f_948:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_948},
{C_text("f_952:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_952},
{C_text("f_956:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_956},
{C_text("f_964:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_964},
{C_text("f_967:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_967},
{C_text("f_975:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_975},
{C_text("f_979:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_979},
{C_text("f_983:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_983},
{C_text("f_991:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_991},
{C_text("f_995:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_995},
{C_text("f_999:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)f_999},
{C_text("toplevel:analysis_2fhelpers_2fsorted_2dslices_2escm"),(void*)C_toplevel},
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
S|  chicken.format#fprintf		1
o|eliminated procedure checks: 11 
o|specializations:
o|  1 (scheme#- fixnum fixnum)
o|  1 (##sys#check-output-port * * *)
(o e)|safe calls: 45 
o|substituted constant variable: a607 
o|inlining procedure: "(analysis/helpers/sorted-slices.scm:56) make-slice" 
o|propagated global variable: length231187 l 
o|inlining procedure: "(analysis/helpers/sorted-slices.scm:24) make-slice" 
o|propagated global variable: sorts241195 ∅ 
o|substituted constant variable: a1163 
o|substituted constant variable: a1164 
o|replaced variables: 177 
o|removed binding forms: 61 
o|substituted constant variable: length231194 
o|replaced variables: 7 
o|removed binding forms: 177 
o|removed binding forms: 8 
o|simplifications: ((##core#call . 41)) 
o|  call simplifications:
o|    chicken.bitwise#arithmetic-shift	6
o|    chicken.bitwise#bitwise-ior	6
o|    scheme#vector-ref
o|    scheme#vector-set!
o|    scheme#modulo
o|    scheme#=
o|    chicken.fixnum#fx/
o|    scheme#>
o|    chicken.fixnum#fx*
o|    scheme#+	3
o|    ##sys#list
o|    ##sys#cons
o|    scheme#<
o|    scheme#-	2
o|    srfi-4#u64vector-length
o|    ##sys#check-structure	6
o|    ##sys#block-ref	3
o|    ##sys#structure?
o|    ##sys#make-structure	3
o|contracted procedure: k365 
o|contracted procedure: k374 
o|contracted procedure: k383 
o|contracted procedure: k392 
o|contracted procedure: k401 
o|contracted procedure: k410 
o|contracted procedure: k604 
o|contracted procedure: k621 
o|contracted procedure: k640 
o|contracted procedure: k683 
o|contracted procedure: k679 
o|contracted procedure: k774 
o|contracted procedure: k841 
o|contracted procedure: k915 
o|contracted procedure: k872 
o|contracted procedure: k907 
o|contracted procedure: k958 
o|contracted procedure: k969 
o|contracted procedure: k985 
o|contracted procedure: k1009 
o|contracted procedure: k1017 
o|contracted procedure: k1049 
o|contracted procedure: k1080 
o|contracted procedure: k1112 
o|contracted procedure: k1108 
o|contracted procedure: k1120 
o|contracted procedure: k1116 
o|contracted procedure: k1128 
o|contracted procedure: k1124 
o|contracted procedure: k1136 
o|contracted procedure: k1132 
o|contracted procedure: k1144 
o|contracted procedure: k1140 
o|contracted procedure: k1152 
o|contracted procedure: k1148 
o|contracted procedure: k1156 
o|simplifications: ((let . 3)) 
o|removed binding forms: 36 
o|inlining procedure: "(analysis/helpers/sorted-slices.scm:58) slice-length" 
o|propagated global variable: x19291201 ω 
o|propagated global variable: x19291201 ω 
o|inlining procedure: "(analysis/helpers/sorted-slices.scm:58) slice-vec" 
o|propagated global variable: x19431204 ω 
o|propagated global variable: x19431204 ω 
o|inlining procedure: "(analysis/helpers/sorted-slices.scm:29) slice-sorts-set!" 
o|propagated global variable: x19391207 ω 
o|propagated global variable: x19391207 ω 
o|propagated global variable: y20401208 s 
o|inlining procedure: "(analysis/helpers/sorted-slices.scm:28) slice-vec-set!" 
o|propagated global variable: x19461211 ω 
o|propagated global variable: x19461211 ω 
o|propagated global variable: y20471212 v 
o|inlining procedure: "(analysis/helpers/sorted-slices.scm:27) slice-length-set!" 
o|propagated global variable: x19321215 ω 
o|propagated global variable: x19321215 ω 
o|propagated global variable: y20331216 n 
o|replaced variables: 5 
o|inlining procedure: k755 
o|removed binding forms: 5 
o|contracted procedure: k751 
o|simplifications: ((let . 1)) 
o|removed binding forms: 2 
*/
/* end of file */
