{-# OPTIONS_GHC -w #-}
module UsqlPar where

import Absyn
import UsqlLex
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11
	= HappyTerminal (UsqlLex.Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,174) ([0,16,0,512,0,0,0,22528,32839,0,0,0,0,0,4608,16376,1,8,0,0,0,0,0,0,0,0,18264,128,0,0,0,0,18,0,0,4096,0,45056,142,1,64521,95,0,0,22528,32839,0,64,0,0,0,0,1,32768,1141,8,36528,256,54784,8209,49152,570,4,18264,128,60160,4104,24576,285,2,9132,64,30080,2052,45056,142,1,4566,32,15040,1026,22528,32839,0,2283,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,1,0,40,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,32768,0,0,0,0,0,0,0,0,16,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_usqlParser","Main","Stmt","Names1","Column","Expr","Exprs","Exprs1","Const","and","false","true","or","not","from","num","name","string","select","'='","'+'","'-'","'/'","'%'","'*'","\"<>\"","'<'","'>'","\">=\"","\"<=\"","'('","')'","','","'.'","%eof"]
        bit_start = st * 37
        bit_end = (st + 1) * 37
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..36]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (21) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (21) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (13) = happyShift action_9
action_3 (14) = happyShift action_10
action_3 (16) = happyShift action_11
action_3 (18) = happyShift action_12
action_3 (19) = happyShift action_13
action_3 (20) = happyShift action_14
action_3 (24) = happyShift action_15
action_3 (33) = happyShift action_16
action_3 (7) = happyGoto action_5
action_3 (8) = happyGoto action_6
action_3 (10) = happyGoto action_7
action_3 (11) = happyGoto action_8
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (37) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_7

action_6 (12) = happyShift action_23
action_6 (15) = happyShift action_24
action_6 (22) = happyShift action_25
action_6 (23) = happyShift action_26
action_6 (24) = happyShift action_27
action_6 (25) = happyShift action_28
action_6 (26) = happyShift action_29
action_6 (27) = happyShift action_30
action_6 (28) = happyShift action_31
action_6 (29) = happyShift action_32
action_6 (30) = happyShift action_33
action_6 (31) = happyShift action_34
action_6 (32) = happyShift action_35
action_6 (35) = happyShift action_36
action_6 _ = happyReduce_27

action_7 (17) = happyShift action_22
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_9

action_9 _ = happyReduce_31

action_10 _ = happyReduce_32

action_11 (13) = happyShift action_9
action_11 (14) = happyShift action_10
action_11 (16) = happyShift action_11
action_11 (18) = happyShift action_12
action_11 (19) = happyShift action_13
action_11 (20) = happyShift action_14
action_11 (24) = happyShift action_15
action_11 (33) = happyShift action_16
action_11 (7) = happyGoto action_5
action_11 (8) = happyGoto action_21
action_11 (11) = happyGoto action_8
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_29

action_13 (33) = happyShift action_19
action_13 (36) = happyShift action_20
action_13 _ = happyReduce_5

action_14 _ = happyReduce_33

action_15 (18) = happyShift action_18
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (13) = happyShift action_9
action_16 (14) = happyShift action_10
action_16 (16) = happyShift action_11
action_16 (18) = happyShift action_12
action_16 (19) = happyShift action_13
action_16 (20) = happyShift action_14
action_16 (24) = happyShift action_15
action_16 (33) = happyShift action_16
action_16 (7) = happyGoto action_5
action_16 (8) = happyGoto action_17
action_16 (11) = happyGoto action_8
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (12) = happyShift action_23
action_17 (15) = happyShift action_24
action_17 (22) = happyShift action_25
action_17 (23) = happyShift action_26
action_17 (24) = happyShift action_27
action_17 (25) = happyShift action_28
action_17 (26) = happyShift action_29
action_17 (27) = happyShift action_30
action_17 (28) = happyShift action_31
action_17 (29) = happyShift action_32
action_17 (30) = happyShift action_33
action_17 (31) = happyShift action_34
action_17 (32) = happyShift action_35
action_17 (34) = happyShift action_56
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_30

action_19 (13) = happyShift action_9
action_19 (14) = happyShift action_10
action_19 (16) = happyShift action_11
action_19 (18) = happyShift action_12
action_19 (19) = happyShift action_13
action_19 (20) = happyShift action_14
action_19 (24) = happyShift action_15
action_19 (33) = happyShift action_16
action_19 (7) = happyGoto action_5
action_19 (8) = happyGoto action_6
action_19 (9) = happyGoto action_54
action_19 (10) = happyGoto action_55
action_19 (11) = happyGoto action_8
action_19 _ = happyReduce_25

action_20 (19) = happyShift action_53
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (12) = happyShift action_23
action_21 (15) = happyShift action_24
action_21 (22) = happyShift action_25
action_21 (23) = happyShift action_26
action_21 (24) = happyShift action_27
action_21 (25) = happyShift action_28
action_21 (26) = happyShift action_29
action_21 (27) = happyShift action_30
action_21 (28) = happyShift action_31
action_21 (29) = happyShift action_32
action_21 (30) = happyShift action_33
action_21 (31) = happyShift action_34
action_21 (32) = happyShift action_35
action_21 _ = happyReduce_11

action_22 (19) = happyShift action_52
action_22 (6) = happyGoto action_51
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (13) = happyShift action_9
action_23 (14) = happyShift action_10
action_23 (16) = happyShift action_11
action_23 (18) = happyShift action_12
action_23 (19) = happyShift action_13
action_23 (20) = happyShift action_14
action_23 (24) = happyShift action_15
action_23 (33) = happyShift action_16
action_23 (7) = happyGoto action_5
action_23 (8) = happyGoto action_50
action_23 (11) = happyGoto action_8
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (13) = happyShift action_9
action_24 (14) = happyShift action_10
action_24 (16) = happyShift action_11
action_24 (18) = happyShift action_12
action_24 (19) = happyShift action_13
action_24 (20) = happyShift action_14
action_24 (24) = happyShift action_15
action_24 (33) = happyShift action_16
action_24 (7) = happyGoto action_5
action_24 (8) = happyGoto action_49
action_24 (11) = happyGoto action_8
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (13) = happyShift action_9
action_25 (14) = happyShift action_10
action_25 (16) = happyShift action_11
action_25 (18) = happyShift action_12
action_25 (19) = happyShift action_13
action_25 (20) = happyShift action_14
action_25 (24) = happyShift action_15
action_25 (33) = happyShift action_16
action_25 (7) = happyGoto action_5
action_25 (8) = happyGoto action_48
action_25 (11) = happyGoto action_8
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (13) = happyShift action_9
action_26 (14) = happyShift action_10
action_26 (16) = happyShift action_11
action_26 (18) = happyShift action_12
action_26 (19) = happyShift action_13
action_26 (20) = happyShift action_14
action_26 (24) = happyShift action_15
action_26 (33) = happyShift action_16
action_26 (7) = happyGoto action_5
action_26 (8) = happyGoto action_47
action_26 (11) = happyGoto action_8
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (13) = happyShift action_9
action_27 (14) = happyShift action_10
action_27 (16) = happyShift action_11
action_27 (18) = happyShift action_12
action_27 (19) = happyShift action_13
action_27 (20) = happyShift action_14
action_27 (24) = happyShift action_15
action_27 (33) = happyShift action_16
action_27 (7) = happyGoto action_5
action_27 (8) = happyGoto action_46
action_27 (11) = happyGoto action_8
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (13) = happyShift action_9
action_28 (14) = happyShift action_10
action_28 (16) = happyShift action_11
action_28 (18) = happyShift action_12
action_28 (19) = happyShift action_13
action_28 (20) = happyShift action_14
action_28 (24) = happyShift action_15
action_28 (33) = happyShift action_16
action_28 (7) = happyGoto action_5
action_28 (8) = happyGoto action_45
action_28 (11) = happyGoto action_8
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (13) = happyShift action_9
action_29 (14) = happyShift action_10
action_29 (16) = happyShift action_11
action_29 (18) = happyShift action_12
action_29 (19) = happyShift action_13
action_29 (20) = happyShift action_14
action_29 (24) = happyShift action_15
action_29 (33) = happyShift action_16
action_29 (7) = happyGoto action_5
action_29 (8) = happyGoto action_44
action_29 (11) = happyGoto action_8
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (13) = happyShift action_9
action_30 (14) = happyShift action_10
action_30 (16) = happyShift action_11
action_30 (18) = happyShift action_12
action_30 (19) = happyShift action_13
action_30 (20) = happyShift action_14
action_30 (24) = happyShift action_15
action_30 (33) = happyShift action_16
action_30 (7) = happyGoto action_5
action_30 (8) = happyGoto action_43
action_30 (11) = happyGoto action_8
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (13) = happyShift action_9
action_31 (14) = happyShift action_10
action_31 (16) = happyShift action_11
action_31 (18) = happyShift action_12
action_31 (19) = happyShift action_13
action_31 (20) = happyShift action_14
action_31 (24) = happyShift action_15
action_31 (33) = happyShift action_16
action_31 (7) = happyGoto action_5
action_31 (8) = happyGoto action_42
action_31 (11) = happyGoto action_8
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (13) = happyShift action_9
action_32 (14) = happyShift action_10
action_32 (16) = happyShift action_11
action_32 (18) = happyShift action_12
action_32 (19) = happyShift action_13
action_32 (20) = happyShift action_14
action_32 (24) = happyShift action_15
action_32 (33) = happyShift action_16
action_32 (7) = happyGoto action_5
action_32 (8) = happyGoto action_41
action_32 (11) = happyGoto action_8
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (13) = happyShift action_9
action_33 (14) = happyShift action_10
action_33 (16) = happyShift action_11
action_33 (18) = happyShift action_12
action_33 (19) = happyShift action_13
action_33 (20) = happyShift action_14
action_33 (24) = happyShift action_15
action_33 (33) = happyShift action_16
action_33 (7) = happyGoto action_5
action_33 (8) = happyGoto action_40
action_33 (11) = happyGoto action_8
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (13) = happyShift action_9
action_34 (14) = happyShift action_10
action_34 (16) = happyShift action_11
action_34 (18) = happyShift action_12
action_34 (19) = happyShift action_13
action_34 (20) = happyShift action_14
action_34 (24) = happyShift action_15
action_34 (33) = happyShift action_16
action_34 (7) = happyGoto action_5
action_34 (8) = happyGoto action_39
action_34 (11) = happyGoto action_8
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (13) = happyShift action_9
action_35 (14) = happyShift action_10
action_35 (16) = happyShift action_11
action_35 (18) = happyShift action_12
action_35 (19) = happyShift action_13
action_35 (20) = happyShift action_14
action_35 (24) = happyShift action_15
action_35 (33) = happyShift action_16
action_35 (7) = happyGoto action_5
action_35 (8) = happyGoto action_38
action_35 (11) = happyGoto action_8
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (13) = happyShift action_9
action_36 (14) = happyShift action_10
action_36 (16) = happyShift action_11
action_36 (18) = happyShift action_12
action_36 (19) = happyShift action_13
action_36 (20) = happyShift action_14
action_36 (24) = happyShift action_15
action_36 (33) = happyShift action_16
action_36 (7) = happyGoto action_5
action_36 (8) = happyGoto action_6
action_36 (10) = happyGoto action_37
action_36 (11) = happyGoto action_8
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_28

action_38 (12) = happyShift action_23
action_38 (15) = happyShift action_24
action_38 (22) = happyShift action_25
action_38 (23) = happyShift action_26
action_38 (24) = happyShift action_27
action_38 (25) = happyShift action_28
action_38 (26) = happyShift action_29
action_38 (27) = happyShift action_30
action_38 (28) = happyShift action_31
action_38 (29) = happyShift action_32
action_38 (30) = happyShift action_33
action_38 (31) = happyShift action_34
action_38 (32) = happyShift action_35
action_38 _ = happyReduce_22

action_39 (12) = happyShift action_23
action_39 (15) = happyShift action_24
action_39 (22) = happyShift action_25
action_39 (23) = happyShift action_26
action_39 (24) = happyShift action_27
action_39 (25) = happyShift action_28
action_39 (26) = happyShift action_29
action_39 (27) = happyShift action_30
action_39 (28) = happyShift action_31
action_39 (29) = happyShift action_32
action_39 (30) = happyShift action_33
action_39 (31) = happyShift action_34
action_39 (32) = happyShift action_35
action_39 _ = happyReduce_21

action_40 (12) = happyShift action_23
action_40 (15) = happyShift action_24
action_40 (22) = happyShift action_25
action_40 (23) = happyShift action_26
action_40 (24) = happyShift action_27
action_40 (25) = happyShift action_28
action_40 (26) = happyShift action_29
action_40 (27) = happyShift action_30
action_40 (28) = happyShift action_31
action_40 (29) = happyShift action_32
action_40 (30) = happyShift action_33
action_40 (31) = happyShift action_34
action_40 (32) = happyShift action_35
action_40 _ = happyReduce_19

action_41 (12) = happyShift action_23
action_41 (15) = happyShift action_24
action_41 (22) = happyShift action_25
action_41 (23) = happyShift action_26
action_41 (24) = happyShift action_27
action_41 (25) = happyShift action_28
action_41 (26) = happyShift action_29
action_41 (27) = happyShift action_30
action_41 (28) = happyShift action_31
action_41 (29) = happyShift action_32
action_41 (30) = happyShift action_33
action_41 (31) = happyShift action_34
action_41 (32) = happyShift action_35
action_41 _ = happyReduce_20

action_42 (12) = happyShift action_23
action_42 (15) = happyShift action_24
action_42 (22) = happyShift action_25
action_42 (23) = happyShift action_26
action_42 (24) = happyShift action_27
action_42 (25) = happyShift action_28
action_42 (26) = happyShift action_29
action_42 (27) = happyShift action_30
action_42 (28) = happyShift action_31
action_42 (29) = happyShift action_32
action_42 (30) = happyShift action_33
action_42 (31) = happyShift action_34
action_42 (32) = happyShift action_35
action_42 _ = happyReduce_18

action_43 (12) = happyShift action_23
action_43 (15) = happyShift action_24
action_43 (22) = happyShift action_25
action_43 (26) = happyShift action_29
action_43 (28) = happyShift action_31
action_43 (29) = happyShift action_32
action_43 (30) = happyShift action_33
action_43 (31) = happyShift action_34
action_43 (32) = happyShift action_35
action_43 _ = happyReduce_14

action_44 (12) = happyShift action_23
action_44 (15) = happyShift action_24
action_44 (22) = happyShift action_25
action_44 (23) = happyShift action_26
action_44 (24) = happyShift action_27
action_44 (25) = happyShift action_28
action_44 (26) = happyShift action_29
action_44 (27) = happyShift action_30
action_44 (28) = happyShift action_31
action_44 (29) = happyShift action_32
action_44 (30) = happyShift action_33
action_44 (31) = happyShift action_34
action_44 (32) = happyShift action_35
action_44 _ = happyReduce_16

action_45 (12) = happyShift action_23
action_45 (15) = happyShift action_24
action_45 (22) = happyShift action_25
action_45 (26) = happyShift action_29
action_45 (28) = happyShift action_31
action_45 (29) = happyShift action_32
action_45 (30) = happyShift action_33
action_45 (31) = happyShift action_34
action_45 (32) = happyShift action_35
action_45 _ = happyReduce_15

action_46 (12) = happyShift action_23
action_46 (15) = happyShift action_24
action_46 (22) = happyShift action_25
action_46 (25) = happyShift action_28
action_46 (26) = happyShift action_29
action_46 (27) = happyShift action_30
action_46 (28) = happyShift action_31
action_46 (29) = happyShift action_32
action_46 (30) = happyShift action_33
action_46 (31) = happyShift action_34
action_46 (32) = happyShift action_35
action_46 _ = happyReduce_13

action_47 (12) = happyShift action_23
action_47 (15) = happyShift action_24
action_47 (22) = happyShift action_25
action_47 (25) = happyShift action_28
action_47 (26) = happyShift action_29
action_47 (27) = happyShift action_30
action_47 (28) = happyShift action_31
action_47 (29) = happyShift action_32
action_47 (30) = happyShift action_33
action_47 (31) = happyShift action_34
action_47 (32) = happyShift action_35
action_47 _ = happyReduce_12

action_48 (12) = happyShift action_23
action_48 (15) = happyShift action_24
action_48 (22) = happyShift action_25
action_48 (23) = happyShift action_26
action_48 (24) = happyShift action_27
action_48 (25) = happyShift action_28
action_48 (26) = happyShift action_29
action_48 (27) = happyShift action_30
action_48 (28) = happyShift action_31
action_48 (29) = happyShift action_32
action_48 (30) = happyShift action_33
action_48 (31) = happyShift action_34
action_48 (32) = happyShift action_35
action_48 _ = happyReduce_17

action_49 (12) = happyShift action_23
action_49 (15) = happyShift action_24
action_49 (22) = happyShift action_25
action_49 (23) = happyShift action_26
action_49 (24) = happyShift action_27
action_49 (25) = happyShift action_28
action_49 (26) = happyShift action_29
action_49 (27) = happyShift action_30
action_49 (28) = happyShift action_31
action_49 (29) = happyShift action_32
action_49 (30) = happyShift action_33
action_49 (31) = happyShift action_34
action_49 (32) = happyShift action_35
action_49 _ = happyReduce_24

action_50 (12) = happyShift action_23
action_50 (15) = happyShift action_24
action_50 (22) = happyShift action_25
action_50 (23) = happyShift action_26
action_50 (24) = happyShift action_27
action_50 (25) = happyShift action_28
action_50 (26) = happyShift action_29
action_50 (27) = happyShift action_30
action_50 (28) = happyShift action_31
action_50 (29) = happyShift action_32
action_50 (30) = happyShift action_33
action_50 (31) = happyShift action_34
action_50 (32) = happyShift action_35
action_50 _ = happyReduce_23

action_51 _ = happyReduce_2

action_52 (35) = happyShift action_58
action_52 _ = happyReduce_3

action_53 _ = happyReduce_6

action_54 (34) = happyShift action_57
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_26

action_56 _ = happyReduce_10

action_57 _ = happyReduce_8

action_58 (19) = happyShift action_52
action_58 (6) = happyGoto action_59
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_4

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happyReduce 4 5 happyReduction_2
happyReduction_2 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Select happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_3)
	_
	(HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn6
		 ((happy_var_1:happy_var_3)
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  7 happyReduction_5
happyReduction_5 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn7
		 (Column happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 (HappyTerminal (TokenName happy_var_3))
	_
	(HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn7
		 (TableColumn happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  8 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn8
		 (ColumnExpr happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happyReduce 4 8 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Prim happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn8
		 (Cst happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  8 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  8 happyReduction_11
happyReduction_11 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Prim "!" [happy_var_2]
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  8 happyReduction_12
happyReduction_12 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Prim "+" [happy_var_1, happy_var_3]
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  8 happyReduction_13
happyReduction_13 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Prim "-" [happy_var_1, happy_var_3]
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  8 happyReduction_14
happyReduction_14 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Prim "*" [happy_var_1, happy_var_3]
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  8 happyReduction_15
happyReduction_15 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Prim "/" [happy_var_1, happy_var_3]
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  8 happyReduction_16
happyReduction_16 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Prim "%" [happy_var_1, happy_var_3]
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  8 happyReduction_17
happyReduction_17 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Prim "==" [happy_var_1, happy_var_3]
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  8 happyReduction_18
happyReduction_18 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Prim "<>" [happy_var_1, happy_var_3]
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  8 happyReduction_19
happyReduction_19 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Prim ">" [happy_var_1, happy_var_3]
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  8 happyReduction_20
happyReduction_20 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Prim "<" [happy_var_1, happy_var_3]
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  8 happyReduction_21
happyReduction_21 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Prim ">=" [happy_var_1, happy_var_3]
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  8 happyReduction_22
happyReduction_22 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Prim "<=" [happy_var_1, happy_var_3]
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  8 happyReduction_23
happyReduction_23 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Prim "&&" [happy_var_1, happy_var_3]
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  8 happyReduction_24
happyReduction_24 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Prim "||" [happy_var_1, happy_var_3]
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_0  9 happyReduction_25
happyReduction_25  =  HappyAbsSyn9
		 ([]
	)

happyReduce_26 = happySpecReduce_1  9 happyReduction_26
happyReduction_26 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  10 happyReduction_27
happyReduction_27 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  10 happyReduction_28
happyReduction_28 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn10
		 ((happy_var_1:happy_var_3)
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  11 happyReduction_29
happyReduction_29 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn11
		 (CstI happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_2  11 happyReduction_30
happyReduction_30 (HappyTerminal (TokenNum happy_var_2))
	_
	 =  HappyAbsSyn11
		 (CstI (-happy_var_2)
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  11 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn11
		 (CstB False
	)

happyReduce_32 = happySpecReduce_1  11 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn11
		 (CstB True
	)

happyReduce_33 = happySpecReduce_1  11 happyReduction_33
happyReduction_33 (HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn11
		 (CstS happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 37 37 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenAnd -> cont 12;
	TokenBool False -> cont 13;
	TokenBool True -> cont 14;
	TokenOr -> cont 15;
	TokenNot -> cont 16;
	TokenFrom -> cont 17;
	TokenNum happy_dollar_dollar -> cont 18;
	TokenName happy_dollar_dollar -> cont 19;
	TokenString happy_dollar_dollar -> cont 20;
	TokenSelect -> cont 21;
	TokenEq -> cont 22;
	TokenAdd -> cont 23;
	TokenSub -> cont 24;
	TokenDiv -> cont 25;
	TokenMod -> cont 26;
	TokenMul -> cont 27;
	TokenNE -> cont 28;
	TokenLT -> cont 29;
	TokenGT -> cont 30;
	TokenGE -> cont 31;
	TokenLE -> cont 32;
	TokenLPar -> cont 33;
	TokenRPar -> cont 34;
	TokenComma -> cont 35;
	TokenDot -> cont 36;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 37 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(UsqlLex.Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
usqlParser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a 
parseError _ = error "Parse error"
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "D:/GitHub/haskell-platform/build/ghc-bindist/local/lib/include/ghcversion.h" #-}















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "F:/Users/randy/AppData/Local/Temp/ghc1900_0/ghc_2.h" #-}


























































































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates\\\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates\\\\GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 75 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 84 "templates\\\\GenericTemplate.hs" #-}

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

{-# LINE 137 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 147 "templates\\\\GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






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
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates\\\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates\\\\GenericTemplate.hs" #-}
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
