{-# OPTIONS_GHC -w #-}
module MJParse where
import MJLex
import MJTokens
import MJAbsSyn

-- parser produced by Happy Version 1.18.9

data HappyAbsSyn 
	= HappyTerminal ((Token AlexPosn))
	| HappyErrorToken Int
	| HappyAbsSyn5 (Prg)
	| HappyAbsSyn6 (MainClass)
	| HappyAbsSyn7 (ClassDeclaration)
	| HappyAbsSyn8 ([ClassDeclaration])
	| HappyAbsSyn9 (VarDeclaration)
	| HappyAbsSyn10 ([VarDeclaration])
	| HappyAbsSyn11 (MethodDeclaration)
	| HappyAbsSyn12 ([MethodDeclaration])
	| HappyAbsSyn13 ([(Type, Identifier)])
	| HappyAbsSyn14 (Type)
	| HappyAbsSyn15 (Stm)
	| HappyAbsSyn17 ([Stm])
	| HappyAbsSyn18 (Exp)
	| HappyAbsSyn19 ([Exp])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> ((Token AlexPosn))
	-> HappyState ((Token AlexPosn)) (HappyStk HappyAbsSyn -> [((Token AlexPosn))] -> m HappyAbsSyn)
	-> [HappyState ((Token AlexPosn)) (HappyStk HappyAbsSyn -> [((Token AlexPosn))] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [((Token AlexPosn))] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> ((Token AlexPosn))
	-> HappyState ((Token AlexPosn)) (HappyStk HappyAbsSyn -> [((Token AlexPosn))] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState ((Token AlexPosn)) (HappyStk HappyAbsSyn -> [((Token AlexPosn))] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [((Token AlexPosn))] -> (HappyIdentity) HappyAbsSyn)

happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> ((Token AlexPosn))
	-> HappyState ((Token AlexPosn)) (HappyStk HappyAbsSyn -> [((Token AlexPosn))] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState ((Token AlexPosn)) (HappyStk HappyAbsSyn -> [((Token AlexPosn))] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [((Token AlexPosn))] -> (HappyIdentity) HappyAbsSyn)

action_0 (27) = happyShift action_4
action_0 (5) = happyGoto action_6
action_0 (6) = happyGoto action_3
action_0 _ = happyFail

action_1 (27) = happyShift action_4
action_1 (5) = happyGoto action_5
action_1 (6) = happyGoto action_3
action_1 _ = happyFail

action_2 (27) = happyShift action_4
action_2 (6) = happyGoto action_3
action_2 _ = happyFail

action_3 (27) = happyShift action_10
action_3 (7) = happyGoto action_8
action_3 (8) = happyGoto action_9
action_3 _ = happyReduce_6

action_4 (59) = happyShift action_7
action_4 _ = happyFail

action_5 (60) = happyAccept
action_5 _ = happyFail

action_6 (60) = happyAccept
action_6 _ = happyFail

action_7 (34) = happyShift action_13
action_7 _ = happyFail

action_8 (27) = happyShift action_10
action_8 (7) = happyGoto action_8
action_8 (8) = happyGoto action_12
action_8 _ = happyReduce_6

action_9 _ = happyReduce_2

action_10 (59) = happyShift action_11
action_10 _ = happyFail

action_11 (28) = happyShift action_15
action_11 (34) = happyShift action_16
action_11 _ = happyFail

action_12 _ = happyReduce_7

action_13 (23) = happyShift action_14
action_13 _ = happyFail

action_14 (24) = happyShift action_19
action_14 _ = happyFail

action_15 (59) = happyShift action_18
action_15 _ = happyFail

action_16 (10) = happyGoto action_17
action_16 _ = happyReduce_9

action_17 (20) = happyShift action_26
action_17 (21) = happyShift action_27
action_17 (23) = happyShift action_28
action_17 (59) = happyShift action_29
action_17 (9) = happyGoto action_22
action_17 (11) = happyGoto action_23
action_17 (12) = happyGoto action_24
action_17 (14) = happyGoto action_25
action_17 _ = happyReduce_13

action_18 (34) = happyShift action_21
action_18 _ = happyFail

action_19 (25) = happyShift action_20
action_19 _ = happyFail

action_20 (26) = happyShift action_36
action_20 _ = happyFail

action_21 (10) = happyGoto action_35
action_21 _ = happyReduce_9

action_22 _ = happyReduce_10

action_23 (23) = happyShift action_28
action_23 (11) = happyGoto action_23
action_23 (12) = happyGoto action_34
action_23 _ = happyReduce_13

action_24 (35) = happyShift action_33
action_24 _ = happyFail

action_25 (59) = happyShift action_32
action_25 _ = happyFail

action_26 (36) = happyShift action_31
action_26 _ = happyReduce_18

action_27 _ = happyReduce_19

action_28 (20) = happyShift action_26
action_28 (21) = happyShift action_27
action_28 (59) = happyShift action_29
action_28 (14) = happyGoto action_30
action_28 _ = happyFail

action_29 _ = happyReduce_20

action_30 (59) = happyShift action_41
action_30 _ = happyFail

action_31 (37) = happyShift action_40
action_31 _ = happyFail

action_32 (57) = happyShift action_39
action_32 _ = happyFail

action_33 _ = happyReduce_4

action_34 _ = happyReduce_14

action_35 (20) = happyShift action_26
action_35 (21) = happyShift action_27
action_35 (23) = happyShift action_28
action_35 (59) = happyShift action_29
action_35 (9) = happyGoto action_22
action_35 (11) = happyGoto action_23
action_35 (12) = happyGoto action_38
action_35 (14) = happyGoto action_25
action_35 _ = happyReduce_13

action_36 (38) = happyShift action_37
action_36 _ = happyFail

action_37 (22) = happyShift action_44
action_37 _ = happyFail

action_38 (35) = happyShift action_43
action_38 _ = happyFail

action_39 _ = happyReduce_8

action_40 _ = happyReduce_17

action_41 (38) = happyShift action_42
action_41 _ = happyFail

action_42 (20) = happyShift action_26
action_42 (21) = happyShift action_27
action_42 (39) = happyShift action_48
action_42 (59) = happyShift action_29
action_42 (13) = happyGoto action_46
action_42 (14) = happyGoto action_47
action_42 _ = happyFail

action_43 _ = happyReduce_5

action_44 (36) = happyShift action_45
action_44 _ = happyFail

action_45 (37) = happyShift action_52
action_45 _ = happyFail

action_46 (39) = happyShift action_51
action_46 _ = happyFail

action_47 (59) = happyShift action_50
action_47 _ = happyFail

action_48 (34) = happyShift action_49
action_48 _ = happyFail

action_49 (10) = happyGoto action_56
action_49 _ = happyReduce_9

action_50 (55) = happyShift action_55
action_50 _ = happyReduce_15

action_51 (34) = happyShift action_54
action_51 _ = happyFail

action_52 (59) = happyShift action_53
action_52 _ = happyFail

action_53 (39) = happyShift action_68
action_53 _ = happyFail

action_54 (10) = happyGoto action_67
action_54 _ = happyReduce_9

action_55 (20) = happyShift action_26
action_55 (21) = happyShift action_27
action_55 (59) = happyShift action_29
action_55 (13) = happyGoto action_66
action_55 (14) = happyGoto action_47
action_55 _ = happyFail

action_56 (20) = happyShift action_26
action_56 (21) = happyShift action_27
action_56 (34) = happyShift action_60
action_56 (40) = happyShift action_61
action_56 (42) = happyShift action_62
action_56 (43) = happyShift action_63
action_56 (44) = happyShift action_64
action_56 (59) = happyShift action_65
action_56 (9) = happyGoto action_22
action_56 (14) = happyGoto action_25
action_56 (15) = happyGoto action_57
action_56 (16) = happyGoto action_58
action_56 (17) = happyGoto action_59
action_56 _ = happyReduce_29

action_57 (34) = happyShift action_60
action_57 (40) = happyShift action_61
action_57 (42) = happyShift action_62
action_57 (43) = happyShift action_63
action_57 (44) = happyShift action_64
action_57 (59) = happyShift action_78
action_57 (15) = happyGoto action_57
action_57 (17) = happyGoto action_80
action_57 _ = happyReduce_29

action_58 (29) = happyShift action_79
action_58 _ = happyFail

action_59 _ = happyReduce_28

action_60 (34) = happyShift action_60
action_60 (40) = happyShift action_61
action_60 (42) = happyShift action_62
action_60 (43) = happyShift action_63
action_60 (44) = happyShift action_64
action_60 (59) = happyShift action_78
action_60 (15) = happyGoto action_57
action_60 (17) = happyGoto action_77
action_60 _ = happyReduce_29

action_61 (38) = happyShift action_76
action_61 _ = happyFail

action_62 (38) = happyShift action_75
action_62 _ = happyFail

action_63 (38) = happyShift action_74
action_63 _ = happyFail

action_64 (38) = happyShift action_73
action_64 _ = happyFail

action_65 (36) = happyShift action_71
action_65 (45) = happyShift action_72
action_65 _ = happyReduce_20

action_66 _ = happyReduce_16

action_67 (20) = happyShift action_26
action_67 (21) = happyShift action_27
action_67 (34) = happyShift action_60
action_67 (40) = happyShift action_61
action_67 (42) = happyShift action_62
action_67 (43) = happyShift action_63
action_67 (44) = happyShift action_64
action_67 (59) = happyShift action_65
action_67 (9) = happyGoto action_22
action_67 (14) = happyGoto action_25
action_67 (15) = happyGoto action_57
action_67 (16) = happyGoto action_70
action_67 (17) = happyGoto action_59
action_67 _ = happyReduce_29

action_68 (34) = happyShift action_69
action_68 _ = happyFail

action_69 (34) = happyShift action_60
action_69 (40) = happyShift action_61
action_69 (42) = happyShift action_62
action_69 (43) = happyShift action_63
action_69 (44) = happyShift action_64
action_69 (59) = happyShift action_78
action_69 (15) = happyGoto action_57
action_69 (16) = happyGoto action_98
action_69 (17) = happyGoto action_59
action_69 _ = happyReduce_29

action_70 (29) = happyShift action_97
action_70 _ = happyFail

action_71 (31) = happyShift action_82
action_71 (32) = happyShift action_83
action_71 (38) = happyShift action_84
action_71 (52) = happyShift action_85
action_71 (53) = happyShift action_86
action_71 (54) = happyShift action_87
action_71 (58) = happyShift action_88
action_71 (59) = happyShift action_89
action_71 (18) = happyGoto action_96
action_71 _ = happyFail

action_72 (31) = happyShift action_82
action_72 (32) = happyShift action_83
action_72 (38) = happyShift action_84
action_72 (52) = happyShift action_85
action_72 (53) = happyShift action_86
action_72 (54) = happyShift action_87
action_72 (58) = happyShift action_88
action_72 (59) = happyShift action_89
action_72 (18) = happyGoto action_95
action_72 _ = happyFail

action_73 (31) = happyShift action_82
action_73 (32) = happyShift action_83
action_73 (38) = happyShift action_84
action_73 (52) = happyShift action_85
action_73 (53) = happyShift action_86
action_73 (54) = happyShift action_87
action_73 (58) = happyShift action_88
action_73 (59) = happyShift action_89
action_73 (18) = happyGoto action_94
action_73 _ = happyFail

action_74 (38) = happyShift action_93
action_74 _ = happyFail

action_75 (31) = happyShift action_82
action_75 (32) = happyShift action_83
action_75 (38) = happyShift action_84
action_75 (52) = happyShift action_85
action_75 (53) = happyShift action_86
action_75 (54) = happyShift action_87
action_75 (58) = happyShift action_88
action_75 (59) = happyShift action_89
action_75 (18) = happyGoto action_92
action_75 _ = happyFail

action_76 (31) = happyShift action_82
action_76 (32) = happyShift action_83
action_76 (38) = happyShift action_84
action_76 (52) = happyShift action_85
action_76 (53) = happyShift action_86
action_76 (54) = happyShift action_87
action_76 (58) = happyShift action_88
action_76 (59) = happyShift action_89
action_76 (18) = happyGoto action_91
action_76 _ = happyFail

action_77 (35) = happyShift action_90
action_77 _ = happyFail

action_78 (36) = happyShift action_71
action_78 (45) = happyShift action_72
action_78 _ = happyFail

action_79 (31) = happyShift action_82
action_79 (32) = happyShift action_83
action_79 (38) = happyShift action_84
action_79 (52) = happyShift action_85
action_79 (53) = happyShift action_86
action_79 (54) = happyShift action_87
action_79 (58) = happyShift action_88
action_79 (59) = happyShift action_89
action_79 (18) = happyGoto action_81
action_79 _ = happyFail

action_80 _ = happyReduce_30

action_81 (36) = happyShift action_101
action_81 (46) = happyShift action_103
action_81 (47) = happyShift action_104
action_81 (48) = happyShift action_105
action_81 (49) = happyShift action_106
action_81 (50) = happyShift action_107
action_81 (51) = happyShift action_108
action_81 (56) = happyShift action_109
action_81 (57) = happyShift action_119
action_81 _ = happyFail

action_82 (20) = happyShift action_117
action_82 (59) = happyShift action_118
action_82 _ = happyFail

action_83 _ = happyReduce_45

action_84 (31) = happyShift action_82
action_84 (32) = happyShift action_83
action_84 (38) = happyShift action_84
action_84 (52) = happyShift action_85
action_84 (53) = happyShift action_86
action_84 (54) = happyShift action_87
action_84 (58) = happyShift action_88
action_84 (59) = happyShift action_89
action_84 (18) = happyGoto action_116
action_84 _ = happyFail

action_85 (31) = happyShift action_82
action_85 (32) = happyShift action_83
action_85 (38) = happyShift action_84
action_85 (52) = happyShift action_85
action_85 (53) = happyShift action_86
action_85 (54) = happyShift action_87
action_85 (58) = happyShift action_88
action_85 (59) = happyShift action_89
action_85 (18) = happyGoto action_115
action_85 _ = happyFail

action_86 _ = happyReduce_43

action_87 _ = happyReduce_44

action_88 _ = happyReduce_32

action_89 _ = happyReduce_31

action_90 _ = happyReduce_21

action_91 (36) = happyShift action_101
action_91 (39) = happyShift action_114
action_91 (46) = happyShift action_103
action_91 (47) = happyShift action_104
action_91 (48) = happyShift action_105
action_91 (49) = happyShift action_106
action_91 (50) = happyShift action_107
action_91 (51) = happyShift action_108
action_91 (56) = happyShift action_109
action_91 _ = happyFail

action_92 (36) = happyShift action_101
action_92 (39) = happyShift action_113
action_92 (46) = happyShift action_103
action_92 (47) = happyShift action_104
action_92 (48) = happyShift action_105
action_92 (49) = happyShift action_106
action_92 (50) = happyShift action_107
action_92 (51) = happyShift action_108
action_92 (56) = happyShift action_109
action_92 _ = happyFail

action_93 (33) = happyShift action_112
action_93 _ = happyFail

action_94 (36) = happyShift action_101
action_94 (39) = happyShift action_111
action_94 (46) = happyShift action_103
action_94 (47) = happyShift action_104
action_94 (48) = happyShift action_105
action_94 (49) = happyShift action_106
action_94 (50) = happyShift action_107
action_94 (51) = happyShift action_108
action_94 (56) = happyShift action_109
action_94 _ = happyFail

action_95 (36) = happyShift action_101
action_95 (46) = happyShift action_103
action_95 (47) = happyShift action_104
action_95 (48) = happyShift action_105
action_95 (49) = happyShift action_106
action_95 (50) = happyShift action_107
action_95 (51) = happyShift action_108
action_95 (56) = happyShift action_109
action_95 (57) = happyShift action_110
action_95 _ = happyFail

action_96 (36) = happyShift action_101
action_96 (37) = happyShift action_102
action_96 (46) = happyShift action_103
action_96 (47) = happyShift action_104
action_96 (48) = happyShift action_105
action_96 (49) = happyShift action_106
action_96 (50) = happyShift action_107
action_96 (51) = happyShift action_108
action_96 (56) = happyShift action_109
action_96 _ = happyFail

action_97 (31) = happyShift action_82
action_97 (32) = happyShift action_83
action_97 (38) = happyShift action_84
action_97 (52) = happyShift action_85
action_97 (53) = happyShift action_86
action_97 (54) = happyShift action_87
action_97 (58) = happyShift action_88
action_97 (59) = happyShift action_89
action_97 (18) = happyGoto action_100
action_97 _ = happyFail

action_98 (35) = happyShift action_99
action_98 _ = happyFail

action_99 (35) = happyShift action_139
action_99 _ = happyFail

action_100 (36) = happyShift action_101
action_100 (46) = happyShift action_103
action_100 (47) = happyShift action_104
action_100 (48) = happyShift action_105
action_100 (49) = happyShift action_106
action_100 (50) = happyShift action_107
action_100 (51) = happyShift action_108
action_100 (56) = happyShift action_109
action_100 (57) = happyShift action_138
action_100 _ = happyFail

action_101 (31) = happyShift action_82
action_101 (32) = happyShift action_83
action_101 (38) = happyShift action_84
action_101 (52) = happyShift action_85
action_101 (53) = happyShift action_86
action_101 (54) = happyShift action_87
action_101 (58) = happyShift action_88
action_101 (59) = happyShift action_89
action_101 (18) = happyGoto action_137
action_101 _ = happyFail

action_102 (45) = happyShift action_136
action_102 _ = happyFail

action_103 (31) = happyShift action_82
action_103 (32) = happyShift action_83
action_103 (38) = happyShift action_84
action_103 (52) = happyShift action_85
action_103 (53) = happyShift action_86
action_103 (54) = happyShift action_87
action_103 (58) = happyShift action_88
action_103 (59) = happyShift action_89
action_103 (18) = happyGoto action_135
action_103 _ = happyFail

action_104 (31) = happyShift action_82
action_104 (32) = happyShift action_83
action_104 (38) = happyShift action_84
action_104 (52) = happyShift action_85
action_104 (53) = happyShift action_86
action_104 (54) = happyShift action_87
action_104 (58) = happyShift action_88
action_104 (59) = happyShift action_89
action_104 (18) = happyGoto action_134
action_104 _ = happyFail

action_105 (31) = happyShift action_82
action_105 (32) = happyShift action_83
action_105 (38) = happyShift action_84
action_105 (52) = happyShift action_85
action_105 (53) = happyShift action_86
action_105 (54) = happyShift action_87
action_105 (58) = happyShift action_88
action_105 (59) = happyShift action_89
action_105 (18) = happyGoto action_133
action_105 _ = happyFail

action_106 (31) = happyShift action_82
action_106 (32) = happyShift action_83
action_106 (38) = happyShift action_84
action_106 (52) = happyShift action_85
action_106 (53) = happyShift action_86
action_106 (54) = happyShift action_87
action_106 (58) = happyShift action_88
action_106 (59) = happyShift action_89
action_106 (18) = happyGoto action_132
action_106 _ = happyFail

action_107 (31) = happyShift action_82
action_107 (32) = happyShift action_83
action_107 (38) = happyShift action_84
action_107 (52) = happyShift action_85
action_107 (53) = happyShift action_86
action_107 (54) = happyShift action_87
action_107 (58) = happyShift action_88
action_107 (59) = happyShift action_89
action_107 (18) = happyGoto action_131
action_107 _ = happyFail

action_108 (31) = happyShift action_82
action_108 (32) = happyShift action_83
action_108 (38) = happyShift action_84
action_108 (52) = happyShift action_85
action_108 (53) = happyShift action_86
action_108 (54) = happyShift action_87
action_108 (58) = happyShift action_88
action_108 (59) = happyShift action_89
action_108 (18) = happyGoto action_130
action_108 _ = happyFail

action_109 (30) = happyShift action_128
action_109 (59) = happyShift action_129
action_109 _ = happyFail

action_110 _ = happyReduce_26

action_111 (57) = happyShift action_127
action_111 _ = happyFail

action_112 (39) = happyShift action_126
action_112 _ = happyFail

action_113 (34) = happyShift action_60
action_113 (40) = happyShift action_61
action_113 (42) = happyShift action_62
action_113 (43) = happyShift action_63
action_113 (44) = happyShift action_64
action_113 (59) = happyShift action_78
action_113 (15) = happyGoto action_125
action_113 _ = happyFail

action_114 (34) = happyShift action_60
action_114 (40) = happyShift action_61
action_114 (42) = happyShift action_62
action_114 (43) = happyShift action_63
action_114 (44) = happyShift action_64
action_114 (59) = happyShift action_78
action_114 (15) = happyGoto action_124
action_114 _ = happyFail

action_115 (36) = happyShift action_101
action_115 (56) = happyShift action_109
action_115 _ = happyReduce_48

action_116 (36) = happyShift action_101
action_116 (39) = happyShift action_123
action_116 (46) = happyShift action_103
action_116 (47) = happyShift action_104
action_116 (48) = happyShift action_105
action_116 (49) = happyShift action_106
action_116 (50) = happyShift action_107
action_116 (51) = happyShift action_108
action_116 (56) = happyShift action_109
action_116 _ = happyFail

action_117 (36) = happyShift action_122
action_117 _ = happyFail

action_118 (38) = happyShift action_121
action_118 _ = happyFail

action_119 (35) = happyShift action_120
action_119 _ = happyFail

action_120 _ = happyReduce_11

action_121 (39) = happyShift action_147
action_121 _ = happyFail

action_122 (31) = happyShift action_82
action_122 (32) = happyShift action_83
action_122 (38) = happyShift action_84
action_122 (52) = happyShift action_85
action_122 (53) = happyShift action_86
action_122 (54) = happyShift action_87
action_122 (58) = happyShift action_88
action_122 (59) = happyShift action_89
action_122 (18) = happyGoto action_146
action_122 _ = happyFail

action_123 _ = happyReduce_49

action_124 (41) = happyShift action_145
action_124 _ = happyFail

action_125 _ = happyReduce_22

action_126 (31) = happyShift action_82
action_126 (32) = happyShift action_83
action_126 (38) = happyShift action_84
action_126 (52) = happyShift action_85
action_126 (53) = happyShift action_86
action_126 (54) = happyShift action_87
action_126 (58) = happyShift action_88
action_126 (59) = happyShift action_89
action_126 (18) = happyGoto action_144
action_126 _ = happyFail

action_127 _ = happyReduce_25

action_128 _ = happyReduce_40

action_129 (38) = happyShift action_143
action_129 _ = happyFail

action_130 (36) = happyShift action_101
action_130 (56) = happyShift action_109
action_130 _ = happyReduce_38

action_131 (36) = happyShift action_101
action_131 (56) = happyShift action_109
action_131 _ = happyReduce_37

action_132 (36) = happyShift action_101
action_132 (50) = happyShift action_107
action_132 (51) = happyShift action_108
action_132 (56) = happyShift action_109
action_132 _ = happyReduce_36

action_133 (36) = happyShift action_101
action_133 (50) = happyShift action_107
action_133 (51) = happyShift action_108
action_133 (56) = happyShift action_109
action_133 _ = happyReduce_35

action_134 (36) = happyShift action_101
action_134 (48) = happyShift action_105
action_134 (49) = happyShift action_106
action_134 (50) = happyShift action_107
action_134 (51) = happyShift action_108
action_134 (56) = happyShift action_109
action_134 _ = happyReduce_34

action_135 (36) = happyShift action_101
action_135 (47) = happyShift action_104
action_135 (48) = happyShift action_105
action_135 (49) = happyShift action_106
action_135 (50) = happyShift action_107
action_135 (51) = happyShift action_108
action_135 (56) = happyShift action_109
action_135 _ = happyReduce_33

action_136 (31) = happyShift action_82
action_136 (32) = happyShift action_83
action_136 (38) = happyShift action_84
action_136 (52) = happyShift action_85
action_136 (53) = happyShift action_86
action_136 (54) = happyShift action_87
action_136 (58) = happyShift action_88
action_136 (59) = happyShift action_89
action_136 (18) = happyGoto action_142
action_136 _ = happyFail

action_137 (36) = happyShift action_101
action_137 (37) = happyShift action_141
action_137 (46) = happyShift action_103
action_137 (47) = happyShift action_104
action_137 (48) = happyShift action_105
action_137 (49) = happyShift action_106
action_137 (50) = happyShift action_107
action_137 (51) = happyShift action_108
action_137 (56) = happyShift action_109
action_137 _ = happyFail

action_138 (35) = happyShift action_140
action_138 _ = happyFail

action_139 _ = happyReduce_3

action_140 _ = happyReduce_12

action_141 _ = happyReduce_39

action_142 (36) = happyShift action_101
action_142 (46) = happyShift action_103
action_142 (47) = happyShift action_104
action_142 (48) = happyShift action_105
action_142 (49) = happyShift action_106
action_142 (50) = happyShift action_107
action_142 (51) = happyShift action_108
action_142 (56) = happyShift action_109
action_142 (57) = happyShift action_154
action_142 _ = happyFail

action_143 (31) = happyShift action_82
action_143 (32) = happyShift action_83
action_143 (38) = happyShift action_84
action_143 (39) = happyShift action_153
action_143 (52) = happyShift action_85
action_143 (53) = happyShift action_86
action_143 (54) = happyShift action_87
action_143 (58) = happyShift action_88
action_143 (59) = happyShift action_89
action_143 (18) = happyGoto action_151
action_143 (19) = happyGoto action_152
action_143 _ = happyFail

action_144 (36) = happyShift action_101
action_144 (39) = happyShift action_150
action_144 (46) = happyShift action_103
action_144 (47) = happyShift action_104
action_144 (48) = happyShift action_105
action_144 (49) = happyShift action_106
action_144 (50) = happyShift action_107
action_144 (51) = happyShift action_108
action_144 (56) = happyShift action_109
action_144 _ = happyFail

action_145 (34) = happyShift action_60
action_145 (40) = happyShift action_61
action_145 (42) = happyShift action_62
action_145 (43) = happyShift action_63
action_145 (44) = happyShift action_64
action_145 (59) = happyShift action_78
action_145 (15) = happyGoto action_149
action_145 _ = happyFail

action_146 (36) = happyShift action_101
action_146 (37) = happyShift action_148
action_146 (46) = happyShift action_103
action_146 (47) = happyShift action_104
action_146 (48) = happyShift action_105
action_146 (49) = happyShift action_106
action_146 (50) = happyShift action_107
action_146 (51) = happyShift action_108
action_146 (56) = happyShift action_109
action_146 _ = happyFail

action_147 _ = happyReduce_47

action_148 _ = happyReduce_46

action_149 _ = happyReduce_23

action_150 (57) = happyShift action_157
action_150 _ = happyFail

action_151 (36) = happyShift action_101
action_151 (46) = happyShift action_103
action_151 (47) = happyShift action_104
action_151 (48) = happyShift action_105
action_151 (49) = happyShift action_106
action_151 (50) = happyShift action_107
action_151 (51) = happyShift action_108
action_151 (55) = happyShift action_156
action_151 (56) = happyShift action_109
action_151 _ = happyReduce_50

action_152 (39) = happyShift action_155
action_152 _ = happyFail

action_153 _ = happyReduce_41

action_154 _ = happyReduce_27

action_155 _ = happyReduce_42

action_156 (31) = happyShift action_82
action_156 (32) = happyShift action_83
action_156 (38) = happyShift action_84
action_156 (52) = happyShift action_85
action_156 (53) = happyShift action_86
action_156 (54) = happyShift action_87
action_156 (58) = happyShift action_88
action_156 (59) = happyShift action_89
action_156 (18) = happyGoto action_151
action_156 (19) = happyGoto action_158
action_156 _ = happyFail

action_157 _ = happyReduce_24

action_158 _ = happyReduce_51

happyReduce_2 = happySpecReduce_2  5 happyReduction_2
happyReduction_2 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (Prg happy_var_1 happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happyReduce 17 6 happyReduction_3
happyReduction_3 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_15) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Id happy_var_12 _)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Id happy_var_2 _)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (MainClass happy_var_2 happy_var_12 happy_var_15
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 6 7 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_5) `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Id happy_var_2 _)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (ClassDeclaration happy_var_2 "" happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 8 7 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_7) `HappyStk`
	(HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Id happy_var_4 _)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Id happy_var_2 _)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (ClassDeclaration happy_var_2 happy_var_4 happy_var_6 happy_var_7
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_0  8 happyReduction_6
happyReduction_6  =  HappyAbsSyn8
		 ([]
	)

happyReduce_7 = happySpecReduce_2  8 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1:happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  9 happyReduction_8
happyReduction_8 _
	(HappyTerminal (Id happy_var_2 _))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn9
		 (VarDeclaration happy_var_1 happy_var_2
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_0  10 happyReduction_9
happyReduction_9  =  HappyAbsSyn10
		 ([]
	)

happyReduce_10 = happySpecReduce_2  10 happyReduction_10
happyReduction_10 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happyReduce 12 11 happyReduction_11
happyReduction_11 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_8) `HappyStk`
	(HappyAbsSyn10  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Id happy_var_3 _)) `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (MethodDeclaration happy_var_2 happy_var_3 [] happy_var_7 happy_var_8 happy_var_10
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 13 11 happyReduction_12
happyReduction_12 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_11) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_9) `HappyStk`
	(HappyAbsSyn10  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Id happy_var_3 _)) `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (MethodDeclaration happy_var_2 happy_var_3 happy_var_5 happy_var_8 happy_var_9 happy_var_11
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_0  12 happyReduction_13
happyReduction_13  =  HappyAbsSyn12
		 ([]
	)

happyReduce_14 = happySpecReduce_2  12 happyReduction_14
happyReduction_14 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1:happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  13 happyReduction_15
happyReduction_15 (HappyTerminal (Id happy_var_2 _))
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 ([(happy_var_1, happy_var_2)]
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 4 13 happyReduction_16
happyReduction_16 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Id happy_var_2 _)) `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 ((happy_var_1, happy_var_2):happy_var_4
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_3  14 happyReduction_17
happyReduction_17 _
	_
	_
	 =  HappyAbsSyn14
		 (IntArrayType
	)

happyReduce_18 = happySpecReduce_1  14 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn14
		 (IntType
	)

happyReduce_19 = happySpecReduce_1  14 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn14
		 (BoolType
	)

happyReduce_20 = happySpecReduce_1  14 happyReduction_20
happyReduction_20 (HappyTerminal (Id happy_var_1 _))
	 =  HappyAbsSyn14
		 (ClassType happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  15 happyReduction_21
happyReduction_21 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (StmList happy_var_2
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 5 15 happyReduction_22
happyReduction_22 ((HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (WhileStm happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 7 15 happyReduction_23
happyReduction_23 ((HappyAbsSyn15  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (IfStm happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 8 15 happyReduction_24
happyReduction_24 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (PrintStm happy_var_6
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 5 15 happyReduction_25
happyReduction_25 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (PrintLnStm happy_var_3
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 4 15 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Id happy_var_1 _)) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (AssignStm happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 7 15 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Id happy_var_1 _)) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (ArrayAssignStm happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_1  16 happyReduction_28
happyReduction_28 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn15
		 (StmList happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_0  17 happyReduction_29
happyReduction_29  =  HappyAbsSyn17
		 ([]
	)

happyReduce_30 = happySpecReduce_2  17 happyReduction_30
happyReduction_30 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1:happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  18 happyReduction_31
happyReduction_31 (HappyTerminal (Id happy_var_1 _))
	 =  HappyAbsSyn18
		 (IdentifierExp happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  18 happyReduction_32
happyReduction_32 (HappyTerminal (IVal happy_var_1 _))
	 =  HappyAbsSyn18
		 (IntExp happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  18 happyReduction_33
happyReduction_33 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (OpExp happy_var_1 MJAbsSyn.OpAnd happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  18 happyReduction_34
happyReduction_34 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (OpExp happy_var_1 MJAbsSyn.OpLt happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  18 happyReduction_35
happyReduction_35 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (OpExp happy_var_1 MJAbsSyn.OpPlus happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  18 happyReduction_36
happyReduction_36 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (OpExp happy_var_1 MJAbsSyn.OpMinus happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  18 happyReduction_37
happyReduction_37 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (OpExp happy_var_1 MJAbsSyn.OpTimes happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  18 happyReduction_38
happyReduction_38 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (OpExp happy_var_1 MJAbsSyn.OpDivide happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happyReduce 4 18 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (ArrayGetExp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_3  18 happyReduction_40
happyReduction_40 _
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (ArrayLengthExp happy_var_1
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happyReduce 5 18 happyReduction_41
happyReduction_41 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Id happy_var_3 _)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (InvokeExp happy_var_1 happy_var_3 []
	) `HappyStk` happyRest

happyReduce_42 = happyReduce 6 18 happyReduction_42
happyReduction_42 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Id happy_var_3 _)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (InvokeExp happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_43 = happySpecReduce_1  18 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn18
		 (BoolExp True
	)

happyReduce_44 = happySpecReduce_1  18 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn18
		 (BoolExp False
	)

happyReduce_45 = happySpecReduce_1  18 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn18
		 (ThisExp
	)

happyReduce_46 = happyReduce 5 18 happyReduction_46
happyReduction_46 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (IntArrayDeclExp happy_var_4
	) `HappyStk` happyRest

happyReduce_47 = happyReduce 4 18 happyReduction_47
happyReduction_47 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Id happy_var_2 _)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (NewObjExp happy_var_2
	) `HappyStk` happyRest

happyReduce_48 = happySpecReduce_2  18 happyReduction_48
happyReduction_48 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (NegExp happy_var_2
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  18 happyReduction_49
happyReduction_49 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (BracedExp happy_var_2
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  19 happyReduction_50
happyReduction_50 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  19 happyReduction_51
happyReduction_51 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1:happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 60 60 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TInt _ -> cont 20;
	TBool _ -> cont 21;
	TString _ -> cont 22;
	Public _ -> cont 23;
	Static _ -> cont 24;
	Void _ -> cont 25;
	Main p -> cont 26;
	Class _ -> cont 27;
	Extends _ -> cont 28;
	Return _ -> cont 29;
	Length _ -> cont 30;
	New _ -> cont 31;
	This _ -> cont 32;
	Char _ -> cont 33;
	LBrace _ -> cont 34;
	RBrace _ -> cont 35;
	LBrack _ -> cont 36;
	RBrack _ -> cont 37;
	LPar _ -> cont 38;
	RPar _ -> cont 39;
	If _ -> cont 40;
	Else _ -> cont 41;
	While _ -> cont 42;
	Print _ -> cont 43;
	PrintLn _ -> cont 44;
	Assign _ -> cont 45;
	MJTokens.OpAnd _ -> cont 46;
	MJTokens.OpLt _ -> cont 47;
	MJTokens.OpPlus _ -> cont 48;
	MJTokens.OpMinus _ -> cont 49;
	MJTokens.OpTimes _ -> cont 50;
	MJTokens.OpDivide _ -> cont 51;
	OpNeg _ -> cont 52;
	CTrue _ -> cont 53;
	CFalse _ -> cont 54;
	Comma _ -> cont 55;
	Point _ -> cont 56;
	Semicol _ -> cont 57;
	IVal happy_dollar_dollar _ -> cont 58;
	Id happy_dollar_dollar _ -> cont 59;
	_ -> happyError' (tk:tks)
	}

happyError_ 60 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [((Token AlexPosn))] -> HappyIdentity a
happyError' = HappyIdentity . happyError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: [Token AlexPosn] -> a
happyError tks = error ("Parse error at " ++ lcn ++ "\n")
	where
	lcn = 	case tks of
		  [] -> "end of file"
		  (tk:_) -> "line " ++ show l ++ ", column " ++ show c ++ " (token " ++ filterPn (show tk) ++ ")"
			where AlexPn _ l c = token_pos tk
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
