{-# OPTIONS_GHC -w #-}
module FunPar where

import Absyn
import FunLex
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

data HappyAbsSyn t4 t5 t6 t7 t8
	= HappyTerminal (FunLex.Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,227) ([11264,8211,22528,16422,0,32752,8192,25,16385,50,2,0,0,0,0,2454,16,512,0,0,0,0,0,39264,256,12992,513,0,0,0,6143,0,0,0,6,0,16377,0,0,0,0,49152,306,32770,613,4,1227,8,2454,16,4908,32,9816,64,19632,128,39264,256,12992,513,25984,1026,51968,2052,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,19632,128,8192,0,12992,513,0,0,1024,2047,38400,4105,256,8188,22528,16422,16384,32752,24576,153,8193,65472,32769,613,4,0,0,65025,15,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_funParser","Main","Expr","AtExpr","AppExpr","Const","else","end","bool","if","in","let","not","then","num","name","'='","'+'","'-'","'/'","'%'","'*'","\"<>\"","'<'","'>'","\">=\"","\"<=\"","'('","')'","eof","%eof"]
        bit_start = st * 33
        bit_end = (st + 1) * 33
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..32]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (11) = happyShift action_6
action_0 (12) = happyShift action_7
action_0 (14) = happyShift action_8
action_0 (17) = happyShift action_9
action_0 (18) = happyShift action_10
action_0 (21) = happyShift action_11
action_0 (30) = happyShift action_12
action_0 (4) = happyGoto action_13
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (11) = happyShift action_6
action_1 (12) = happyShift action_7
action_1 (14) = happyShift action_8
action_1 (17) = happyShift action_9
action_1 (18) = happyShift action_10
action_1 (21) = happyShift action_11
action_1 (30) = happyShift action_12
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (8) = happyGoto action_5
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (19) = happyShift action_20
action_2 (20) = happyShift action_21
action_2 (21) = happyShift action_22
action_2 (22) = happyShift action_23
action_2 (23) = happyShift action_24
action_2 (24) = happyShift action_25
action_2 (25) = happyShift action_26
action_2 (26) = happyShift action_27
action_2 (27) = happyShift action_28
action_2 (28) = happyShift action_29
action_2 (29) = happyShift action_30
action_2 _ = happyReduce_1

action_3 (11) = happyShift action_6
action_3 (14) = happyShift action_8
action_3 (17) = happyShift action_9
action_3 (18) = happyShift action_10
action_3 (30) = happyShift action_12
action_3 (6) = happyGoto action_19
action_3 (8) = happyGoto action_5
action_3 _ = happyReduce_2

action_4 (11) = happyShift action_6
action_4 (14) = happyShift action_8
action_4 (17) = happyShift action_9
action_4 (18) = happyShift action_10
action_4 (30) = happyShift action_12
action_4 (6) = happyGoto action_18
action_4 (8) = happyGoto action_5
action_4 _ = happyReduce_3

action_5 _ = happyReduce_17

action_6 _ = happyReduce_25

action_7 (11) = happyShift action_6
action_7 (12) = happyShift action_7
action_7 (14) = happyShift action_8
action_7 (17) = happyShift action_9
action_7 (18) = happyShift action_10
action_7 (21) = happyShift action_11
action_7 (30) = happyShift action_12
action_7 (5) = happyGoto action_17
action_7 (6) = happyGoto action_3
action_7 (7) = happyGoto action_4
action_7 (8) = happyGoto action_5
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (18) = happyShift action_16
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_24

action_10 _ = happyReduce_18

action_11 (11) = happyShift action_6
action_11 (12) = happyShift action_7
action_11 (14) = happyShift action_8
action_11 (17) = happyShift action_9
action_11 (18) = happyShift action_10
action_11 (21) = happyShift action_11
action_11 (30) = happyShift action_12
action_11 (5) = happyGoto action_15
action_11 (6) = happyGoto action_3
action_11 (7) = happyGoto action_4
action_11 (8) = happyGoto action_5
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (11) = happyShift action_6
action_12 (12) = happyShift action_7
action_12 (14) = happyShift action_8
action_12 (17) = happyShift action_9
action_12 (18) = happyShift action_10
action_12 (21) = happyShift action_11
action_12 (30) = happyShift action_12
action_12 (5) = happyGoto action_14
action_12 (6) = happyGoto action_3
action_12 (7) = happyGoto action_4
action_12 (8) = happyGoto action_5
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (33) = happyAccept
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (19) = happyShift action_20
action_14 (20) = happyShift action_21
action_14 (21) = happyShift action_22
action_14 (22) = happyShift action_23
action_14 (23) = happyShift action_24
action_14 (24) = happyShift action_25
action_14 (25) = happyShift action_26
action_14 (26) = happyShift action_27
action_14 (27) = happyShift action_28
action_14 (28) = happyShift action_29
action_14 (29) = happyShift action_30
action_14 (31) = happyShift action_45
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (19) = happyShift action_20
action_15 (20) = happyShift action_21
action_15 (21) = happyShift action_22
action_15 (22) = happyShift action_23
action_15 (23) = happyShift action_24
action_15 (24) = happyShift action_25
action_15 (25) = happyShift action_26
action_15 (26) = happyShift action_27
action_15 (27) = happyShift action_28
action_15 (28) = happyShift action_29
action_15 (29) = happyShift action_30
action_15 _ = happyReduce_5

action_16 (18) = happyShift action_43
action_16 (19) = happyShift action_44
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (16) = happyShift action_42
action_17 (19) = happyShift action_20
action_17 (20) = happyShift action_21
action_17 (21) = happyShift action_22
action_17 (22) = happyShift action_23
action_17 (23) = happyShift action_24
action_17 (24) = happyShift action_25
action_17 (25) = happyShift action_26
action_17 (26) = happyShift action_27
action_17 (27) = happyShift action_28
action_17 (28) = happyShift action_29
action_17 (29) = happyShift action_30
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_23

action_19 _ = happyReduce_22

action_20 (11) = happyShift action_6
action_20 (12) = happyShift action_7
action_20 (14) = happyShift action_8
action_20 (17) = happyShift action_9
action_20 (18) = happyShift action_10
action_20 (21) = happyShift action_11
action_20 (30) = happyShift action_12
action_20 (5) = happyGoto action_41
action_20 (6) = happyGoto action_3
action_20 (7) = happyGoto action_4
action_20 (8) = happyGoto action_5
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (11) = happyShift action_6
action_21 (12) = happyShift action_7
action_21 (14) = happyShift action_8
action_21 (17) = happyShift action_9
action_21 (18) = happyShift action_10
action_21 (21) = happyShift action_11
action_21 (30) = happyShift action_12
action_21 (5) = happyGoto action_40
action_21 (6) = happyGoto action_3
action_21 (7) = happyGoto action_4
action_21 (8) = happyGoto action_5
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (11) = happyShift action_6
action_22 (12) = happyShift action_7
action_22 (14) = happyShift action_8
action_22 (17) = happyShift action_9
action_22 (18) = happyShift action_10
action_22 (21) = happyShift action_11
action_22 (30) = happyShift action_12
action_22 (5) = happyGoto action_39
action_22 (6) = happyGoto action_3
action_22 (7) = happyGoto action_4
action_22 (8) = happyGoto action_5
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (11) = happyShift action_6
action_23 (12) = happyShift action_7
action_23 (14) = happyShift action_8
action_23 (17) = happyShift action_9
action_23 (18) = happyShift action_10
action_23 (21) = happyShift action_11
action_23 (30) = happyShift action_12
action_23 (5) = happyGoto action_38
action_23 (6) = happyGoto action_3
action_23 (7) = happyGoto action_4
action_23 (8) = happyGoto action_5
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (11) = happyShift action_6
action_24 (12) = happyShift action_7
action_24 (14) = happyShift action_8
action_24 (17) = happyShift action_9
action_24 (18) = happyShift action_10
action_24 (21) = happyShift action_11
action_24 (30) = happyShift action_12
action_24 (5) = happyGoto action_37
action_24 (6) = happyGoto action_3
action_24 (7) = happyGoto action_4
action_24 (8) = happyGoto action_5
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (11) = happyShift action_6
action_25 (12) = happyShift action_7
action_25 (14) = happyShift action_8
action_25 (17) = happyShift action_9
action_25 (18) = happyShift action_10
action_25 (21) = happyShift action_11
action_25 (30) = happyShift action_12
action_25 (5) = happyGoto action_36
action_25 (6) = happyGoto action_3
action_25 (7) = happyGoto action_4
action_25 (8) = happyGoto action_5
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (11) = happyShift action_6
action_26 (12) = happyShift action_7
action_26 (14) = happyShift action_8
action_26 (17) = happyShift action_9
action_26 (18) = happyShift action_10
action_26 (21) = happyShift action_11
action_26 (30) = happyShift action_12
action_26 (5) = happyGoto action_35
action_26 (6) = happyGoto action_3
action_26 (7) = happyGoto action_4
action_26 (8) = happyGoto action_5
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (11) = happyShift action_6
action_27 (12) = happyShift action_7
action_27 (14) = happyShift action_8
action_27 (17) = happyShift action_9
action_27 (18) = happyShift action_10
action_27 (21) = happyShift action_11
action_27 (30) = happyShift action_12
action_27 (5) = happyGoto action_34
action_27 (6) = happyGoto action_3
action_27 (7) = happyGoto action_4
action_27 (8) = happyGoto action_5
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (11) = happyShift action_6
action_28 (12) = happyShift action_7
action_28 (14) = happyShift action_8
action_28 (17) = happyShift action_9
action_28 (18) = happyShift action_10
action_28 (21) = happyShift action_11
action_28 (30) = happyShift action_12
action_28 (5) = happyGoto action_33
action_28 (6) = happyGoto action_3
action_28 (7) = happyGoto action_4
action_28 (8) = happyGoto action_5
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (11) = happyShift action_6
action_29 (12) = happyShift action_7
action_29 (14) = happyShift action_8
action_29 (17) = happyShift action_9
action_29 (18) = happyShift action_10
action_29 (21) = happyShift action_11
action_29 (30) = happyShift action_12
action_29 (5) = happyGoto action_32
action_29 (6) = happyGoto action_3
action_29 (7) = happyGoto action_4
action_29 (8) = happyGoto action_5
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (11) = happyShift action_6
action_30 (12) = happyShift action_7
action_30 (14) = happyShift action_8
action_30 (17) = happyShift action_9
action_30 (18) = happyShift action_10
action_30 (21) = happyShift action_11
action_30 (30) = happyShift action_12
action_30 (5) = happyGoto action_31
action_30 (6) = happyGoto action_3
action_30 (7) = happyGoto action_4
action_30 (8) = happyGoto action_5
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (19) = happyShift action_20
action_31 (20) = happyShift action_21
action_31 (21) = happyShift action_22
action_31 (22) = happyShift action_23
action_31 (23) = happyShift action_24
action_31 (24) = happyShift action_25
action_31 (25) = happyShift action_26
action_31 (26) = happyShift action_27
action_31 (27) = happyShift action_28
action_31 (28) = happyShift action_29
action_31 (29) = happyShift action_30
action_31 _ = happyReduce_16

action_32 (19) = happyShift action_20
action_32 (20) = happyShift action_21
action_32 (21) = happyShift action_22
action_32 (22) = happyShift action_23
action_32 (23) = happyShift action_24
action_32 (24) = happyShift action_25
action_32 (25) = happyShift action_26
action_32 (26) = happyShift action_27
action_32 (27) = happyShift action_28
action_32 (28) = happyShift action_29
action_32 (29) = happyShift action_30
action_32 _ = happyReduce_15

action_33 (19) = happyShift action_20
action_33 (20) = happyShift action_21
action_33 (21) = happyShift action_22
action_33 (22) = happyShift action_23
action_33 (23) = happyShift action_24
action_33 (24) = happyShift action_25
action_33 (25) = happyShift action_26
action_33 (26) = happyShift action_27
action_33 (27) = happyShift action_28
action_33 (28) = happyShift action_29
action_33 (29) = happyShift action_30
action_33 _ = happyReduce_13

action_34 (19) = happyShift action_20
action_34 (20) = happyShift action_21
action_34 (21) = happyShift action_22
action_34 (22) = happyShift action_23
action_34 (23) = happyShift action_24
action_34 (24) = happyShift action_25
action_34 (25) = happyShift action_26
action_34 (26) = happyShift action_27
action_34 (27) = happyShift action_28
action_34 (28) = happyShift action_29
action_34 (29) = happyShift action_30
action_34 _ = happyReduce_14

action_35 (19) = happyShift action_20
action_35 (20) = happyShift action_21
action_35 (21) = happyShift action_22
action_35 (22) = happyShift action_23
action_35 (23) = happyShift action_24
action_35 (24) = happyShift action_25
action_35 (25) = happyShift action_26
action_35 (26) = happyShift action_27
action_35 (27) = happyShift action_28
action_35 (28) = happyShift action_29
action_35 (29) = happyShift action_30
action_35 _ = happyReduce_12

action_36 (19) = happyShift action_20
action_36 (20) = happyShift action_21
action_36 (21) = happyShift action_22
action_36 (22) = happyShift action_23
action_36 (23) = happyShift action_24
action_36 (24) = happyShift action_25
action_36 (25) = happyShift action_26
action_36 (26) = happyShift action_27
action_36 (27) = happyShift action_28
action_36 (28) = happyShift action_29
action_36 (29) = happyShift action_30
action_36 _ = happyReduce_8

action_37 (19) = happyShift action_20
action_37 (20) = happyShift action_21
action_37 (21) = happyShift action_22
action_37 (22) = happyShift action_23
action_37 (23) = happyShift action_24
action_37 (24) = happyShift action_25
action_37 (25) = happyShift action_26
action_37 (26) = happyShift action_27
action_37 (27) = happyShift action_28
action_37 (28) = happyShift action_29
action_37 (29) = happyShift action_30
action_37 _ = happyReduce_10

action_38 (19) = happyShift action_20
action_38 (20) = happyShift action_21
action_38 (21) = happyShift action_22
action_38 (22) = happyShift action_23
action_38 (23) = happyShift action_24
action_38 (24) = happyShift action_25
action_38 (25) = happyShift action_26
action_38 (26) = happyShift action_27
action_38 (27) = happyShift action_28
action_38 (28) = happyShift action_29
action_38 (29) = happyShift action_30
action_38 _ = happyReduce_9

action_39 (19) = happyShift action_20
action_39 (20) = happyShift action_21
action_39 (21) = happyShift action_22
action_39 (22) = happyShift action_23
action_39 (23) = happyShift action_24
action_39 (24) = happyShift action_25
action_39 (25) = happyShift action_26
action_39 (26) = happyShift action_27
action_39 (27) = happyShift action_28
action_39 (28) = happyShift action_29
action_39 (29) = happyShift action_30
action_39 _ = happyReduce_7

action_40 (19) = happyShift action_20
action_40 (20) = happyShift action_21
action_40 (21) = happyShift action_22
action_40 (22) = happyShift action_23
action_40 (23) = happyShift action_24
action_40 (24) = happyShift action_25
action_40 (25) = happyShift action_26
action_40 (26) = happyShift action_27
action_40 (27) = happyShift action_28
action_40 (28) = happyShift action_29
action_40 (29) = happyShift action_30
action_40 _ = happyReduce_6

action_41 (19) = happyShift action_20
action_41 (20) = happyShift action_21
action_41 (21) = happyShift action_22
action_41 (22) = happyShift action_23
action_41 (23) = happyShift action_24
action_41 (24) = happyShift action_25
action_41 (25) = happyShift action_26
action_41 (26) = happyShift action_27
action_41 (27) = happyShift action_28
action_41 (28) = happyShift action_29
action_41 (29) = happyShift action_30
action_41 _ = happyReduce_11

action_42 (11) = happyShift action_6
action_42 (12) = happyShift action_7
action_42 (14) = happyShift action_8
action_42 (17) = happyShift action_9
action_42 (18) = happyShift action_10
action_42 (21) = happyShift action_11
action_42 (30) = happyShift action_12
action_42 (5) = happyGoto action_48
action_42 (6) = happyGoto action_3
action_42 (7) = happyGoto action_4
action_42 (8) = happyGoto action_5
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (19) = happyShift action_47
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (11) = happyShift action_6
action_44 (12) = happyShift action_7
action_44 (14) = happyShift action_8
action_44 (17) = happyShift action_9
action_44 (18) = happyShift action_10
action_44 (21) = happyShift action_11
action_44 (30) = happyShift action_12
action_44 (5) = happyGoto action_46
action_44 (6) = happyGoto action_3
action_44 (7) = happyGoto action_4
action_44 (8) = happyGoto action_5
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_21

action_46 (13) = happyShift action_51
action_46 (19) = happyShift action_20
action_46 (20) = happyShift action_21
action_46 (21) = happyShift action_22
action_46 (22) = happyShift action_23
action_46 (23) = happyShift action_24
action_46 (24) = happyShift action_25
action_46 (25) = happyShift action_26
action_46 (26) = happyShift action_27
action_46 (27) = happyShift action_28
action_46 (28) = happyShift action_29
action_46 (29) = happyShift action_30
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (11) = happyShift action_6
action_47 (12) = happyShift action_7
action_47 (14) = happyShift action_8
action_47 (17) = happyShift action_9
action_47 (18) = happyShift action_10
action_47 (21) = happyShift action_11
action_47 (30) = happyShift action_12
action_47 (5) = happyGoto action_50
action_47 (6) = happyGoto action_3
action_47 (7) = happyGoto action_4
action_47 (8) = happyGoto action_5
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (9) = happyShift action_49
action_48 (19) = happyShift action_20
action_48 (20) = happyShift action_21
action_48 (21) = happyShift action_22
action_48 (22) = happyShift action_23
action_48 (23) = happyShift action_24
action_48 (24) = happyShift action_25
action_48 (25) = happyShift action_26
action_48 (26) = happyShift action_27
action_48 (27) = happyShift action_28
action_48 (28) = happyShift action_29
action_48 (29) = happyShift action_30
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (11) = happyShift action_6
action_49 (12) = happyShift action_7
action_49 (14) = happyShift action_8
action_49 (17) = happyShift action_9
action_49 (18) = happyShift action_10
action_49 (21) = happyShift action_11
action_49 (30) = happyShift action_12
action_49 (5) = happyGoto action_54
action_49 (6) = happyGoto action_3
action_49 (7) = happyGoto action_4
action_49 (8) = happyGoto action_5
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (13) = happyShift action_53
action_50 (19) = happyShift action_20
action_50 (20) = happyShift action_21
action_50 (21) = happyShift action_22
action_50 (22) = happyShift action_23
action_50 (23) = happyShift action_24
action_50 (24) = happyShift action_25
action_50 (25) = happyShift action_26
action_50 (26) = happyShift action_27
action_50 (27) = happyShift action_28
action_50 (28) = happyShift action_29
action_50 (29) = happyShift action_30
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (11) = happyShift action_6
action_51 (12) = happyShift action_7
action_51 (14) = happyShift action_8
action_51 (17) = happyShift action_9
action_51 (18) = happyShift action_10
action_51 (21) = happyShift action_11
action_51 (30) = happyShift action_12
action_51 (5) = happyGoto action_52
action_51 (6) = happyGoto action_3
action_51 (7) = happyGoto action_4
action_51 (8) = happyGoto action_5
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (10) = happyShift action_56
action_52 (19) = happyShift action_20
action_52 (20) = happyShift action_21
action_52 (21) = happyShift action_22
action_52 (22) = happyShift action_23
action_52 (23) = happyShift action_24
action_52 (24) = happyShift action_25
action_52 (25) = happyShift action_26
action_52 (26) = happyShift action_27
action_52 (27) = happyShift action_28
action_52 (28) = happyShift action_29
action_52 (29) = happyShift action_30
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (11) = happyShift action_6
action_53 (12) = happyShift action_7
action_53 (14) = happyShift action_8
action_53 (17) = happyShift action_9
action_53 (18) = happyShift action_10
action_53 (21) = happyShift action_11
action_53 (30) = happyShift action_12
action_53 (5) = happyGoto action_55
action_53 (6) = happyGoto action_3
action_53 (7) = happyGoto action_4
action_53 (8) = happyGoto action_5
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (19) = happyShift action_20
action_54 (20) = happyShift action_21
action_54 (21) = happyShift action_22
action_54 (22) = happyShift action_23
action_54 (23) = happyShift action_24
action_54 (24) = happyShift action_25
action_54 (25) = happyShift action_26
action_54 (26) = happyShift action_27
action_54 (27) = happyShift action_28
action_54 (28) = happyShift action_29
action_54 (29) = happyShift action_30
action_54 _ = happyReduce_4

action_55 (10) = happyShift action_57
action_55 (19) = happyShift action_20
action_55 (20) = happyShift action_21
action_55 (21) = happyShift action_22
action_55 (22) = happyShift action_23
action_55 (23) = happyShift action_24
action_55 (24) = happyShift action_25
action_55 (25) = happyShift action_26
action_55 (26) = happyShift action_27
action_55 (27) = happyShift action_28
action_55 (28) = happyShift action_29
action_55 (29) = happyShift action_30
action_55 _ = happyFail (happyExpListPerState 55)

action_56 _ = happyReduce_19

action_57 _ = happyReduce_20

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happyReduce 6 5 happyReduction_4
happyReduction_4 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_2  5 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Prim "-" (CstI 0) happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Prim "+" happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  5 happyReduction_7
happyReduction_7 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Prim "-" happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  5 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Prim "*" happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  5 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Prim "/" happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  5 happyReduction_10
happyReduction_10 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Prim "%" happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  5 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Prim "=" happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  5 happyReduction_12
happyReduction_12 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Prim "<>" happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  5 happyReduction_13
happyReduction_13 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Prim ">" happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  5 happyReduction_14
happyReduction_14 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Prim "<" happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  5 happyReduction_15
happyReduction_15 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Prim ">=" happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  5 happyReduction_16
happyReduction_16 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Prim "<=" happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  6 happyReduction_17
happyReduction_17 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  6 happyReduction_18
happyReduction_18 (HappyTerminal (TokenName happy_var_1))
	 =  HappyAbsSyn6
		 (Var happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happyReduce 7 6 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Let happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 8 6 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenName happy_var_3)) `HappyStk`
	(HappyTerminal (TokenName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Letfun happy_var_2 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_3  6 happyReduction_21
happyReduction_21 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  7 happyReduction_22
happyReduction_22 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn7
		 (Call happy_var_1 happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  7 happyReduction_23
happyReduction_23 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Call happy_var_1 happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  8 happyReduction_24
happyReduction_24 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn8
		 (CstI happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  8 happyReduction_25
happyReduction_25 (HappyTerminal (TokenBool happy_var_1))
	 =  HappyAbsSyn8
		 (CstB happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 33 33 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenElse -> cont 9;
	TokenEnd -> cont 10;
	TokenBool happy_dollar_dollar -> cont 11;
	TokenIf -> cont 12;
	TokenIn -> cont 13;
	TokenLet -> cont 14;
	TokenNot -> cont 15;
	TokenThen -> cont 16;
	TokenNum happy_dollar_dollar -> cont 17;
	TokenName happy_dollar_dollar -> cont 18;
	TokenEq -> cont 19;
	TokenAdd -> cont 20;
	TokenSub -> cont 21;
	TokenDiv -> cont 22;
	TokenMod -> cont 23;
	TokenMul -> cont 24;
	TokenNE -> cont 25;
	TokenLT -> cont 26;
	TokenGT -> cont 27;
	TokenGE -> cont 28;
	TokenLE -> cont 29;
	TokenLPar -> cont 30;
	TokenRPar -> cont 31;
	TokenEOF -> cont 32;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 33 tk tks = happyError' (tks, explist)
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
happyError' :: () => ([(FunLex.Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
funParser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a 
parseError _ = error "Parse error"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8814_0/ghc_2.h" #-}




























































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

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

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
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

{-# LINE 267 "templates/GenericTemplate.hs" #-}
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

{-# LINE 333 "templates/GenericTemplate.hs" #-}
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
