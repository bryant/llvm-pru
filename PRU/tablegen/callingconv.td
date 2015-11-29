def pru_call_conv : CallingConv<[
    CCIfByVal<CCPassByVal<1, 1>>,

    CCIfConsecutiveRegs<CCCustom<"alloc_reg_block">>,

    CCIfType<[i1], CCPromoteToType<i8>>,

    CCIfType<[i8], CCAssignToReg<[
        r14_b0, r14_b1, r14_b2, r14_b3, r15_b0, r15_b1, r15_b2, r15_b3, r16_b0,
        r16_b1, r16_b2, r16_b3, r17_b0, r17_b1, r17_b2, r17_b3, r18_b0, r18_b1,
        r18_b2, r18_b3, r19_b0, r19_b1, r19_b2, r19_b3, r20_b0, r20_b1, r20_b2,
        r20_b3, r21_b0, r21_b1, r21_b2, r21_b3, r22_b0, r22_b1, r22_b2, r22_b3,
        r23_b0, r23_b1, r23_b2, r23_b3, r24_b0, r24_b1, r24_b2, r24_b3, r25_b0,
        r25_b1, r25_b2, r25_b3, r26_b0, r26_b1, r26_b2, r26_b3, r27_b0, r27_b1,
        r27_b2, r27_b3, r28_b0, r28_b1, r28_b2, r28_b3, r29_b0, r29_b1, r29_b2,
        r29_b3
    ]>>,

    CCIfType<[i16], CCAssignToReg<[
        r14_w0, r14_w1, r14_w2, r15_w0, r15_w1, r15_w2, r16_w0, r16_w1, r16_w2,
        r17_w0, r17_w1, r17_w2, r18_w0, r18_w1, r18_w2, r19_w0, r19_w1, r19_w2,
        r20_w0, r20_w1, r20_w2, r21_w0, r21_w1, r21_w2, r22_w0, r22_w1, r22_w2,
        r23_w0, r23_w1, r23_w2, r24_w0, r24_w1, r24_w2, r25_w0, r25_w1, r25_w2,
        r26_w0, r26_w1, r26_w2, r27_w0, r27_w1, r27_w2, r28_w0, r28_w1, r28_w2,
        r29_w0, r29_w1, r29_w2
    ]>>,

    CCIfType<[i32], CCAssignToReg<[r14, r15, r16, r17, r18, r19, r20, r21, r22,
        r23, r24, r25, r26, r27, r28, r29]>>,

    // couldn't alloc a reg, assign to stack
    CCAssignToStack<0, 0>
]>;

def pru_return_conv : CallingConv<[
    CCIfType<[i1, i8, i16], CCPromoteToType<i32>>,
    CCIfType<[i32], CCAssignToReg<[r14, r15]>>
]>;

def callee_saved : CalleeSavedRegs<(add r3_w2, (sequence "r%d", 4, 13))>;
