package benchmarks.sha512

class SHA512Context(private val is384: Boolean) {

  private val total = longArrayOf(0L, 0L) // number of bytes processed
  private val state = longArrayOf(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L) // intermediate digest state
  // data block being processed (size 128)
  private val buffer = ByteArray(128)

  init {
    if (!is384) {
      // SHA-512
      state[0] = 0x6A09E667F3BCC908L
      state[1] = -0x4498517A7B3558C5L
      state[2] = 0x3C6EF372FE94F82BL
      state[3] = -0x5AB00AC5A0E2C90FL
      state[4] = 0x510E527FADE682D1L
      state[5] = -0x64FA9773D4C193E1L
      state[6] = 0x1F83D9ABFB41BD6BL
      state[7] = 0x5BE0CD19137E2179L
    } else {
      // SHA-384
      state[0] = -0x344462A23EFA6128L
      state[1] = 0x629A292A367CD507L
      state[2] = -0x6EA6FEA5CF8F22E9L
      state[3] = 0x152FECD8F70E5939L
      state[4] = 0x67332667FFC00B31L
      state[5] = -0x714BB57897A7EAEFL
      state[6] = -0x24F3D1F29B067059L
      state[7] = 0x47B5481DBEFA4FA4L
    }
  }
  companion object {
    fun sha512(input: ByteArray, is384: Boolean): ByteArray {
      val ctx = SHA512Context(is384)
      val output = ByteArray(if (is384) 48 else 64)

      ctx.update(input, input.size)
      ctx.finish(output)
      return output
    }

    /*
    * 64-bit integer manipulation macros (big endian)
    */
    fun getUInt64BE(b: ByteArray, i: Int): Long {
      return (
              ((b[i].toLong() and 0xffL) shl 56) or
                      ((b[i + 1].toLong() and 0xffL) shl 48) or
                      ((b[i + 2].toLong() and 0xffL) shl 40) or
                      ((b[i + 3].toLong() and 0xffL) shl 32) or
                      ((b[i + 4].toLong() and 0xffL) shl 24) or
                      ((b[i + 5].toLong() and 0xffL) shl 16) or
                      ((b[i + 6].toLong() and 0xffL) shl 8) or
                      (b[i + 7].toLong() and 0xffL)
              )
    }

    fun putUInt64BE(b: ByteArray, i: Int, n: Long) {
      b[i] = (n shr 56).toByte()
      b[i + 1] = (n shr 48).toByte()
      b[i + 2] = (n shr 40).toByte()
      b[i + 3] = (n shr 32).toByte()
      b[i + 4] = (n shr 24).toByte()
      b[i + 5] = (n shr 16).toByte()
      b[i + 6] = (n shr 8).toByte()
      b[i + 7] = n.toByte()
    }

    /*
    * Round constants
    */
    val K = longArrayOf(
            0x428A2F98D728AE22L,
            0x7137449123EF65CDL,
            -0x4A3F043013B2C4D1L,
            -0x164A245A7E762444L,
            0x3956C25BF348B538L,
            0x59F111F1B605D019L,
            -0x6DC07D5B50E6B065L,
            -0x54E3A12A25927EE8L,
            -0x27F855675CFCFDBEL,
            0x12835B0145706FBEL,
            0x243185BE4EE4B28CL,
            0x550C7DC3D5FFB4E2L,
            0x72BE5D74F27B896FL,
            -0x7F214E01C4E9694FL,
            -0x6423F958DA38EDCBL,
            -0x3E640E8B3096D96CL,
            -0x1B64963E610EB52EL,
            -0x1041B879C7B0DA1DL,
            0xFC19DC68B8CD5B5L,
            0x240CA1CC77AC9C65L,
            0x2DE92C6F592B0275L,
            0x4A7484AA6EA6E483L,
            0x5CB0A9DCBD41FBD4L,
            0x76F988DA831153B5L,
            -0x67C1AEAD11992055L,
            -0x57CE3992D24BCDF0L,
            -0x4FFCD8376704DEC1L,
            -0x40A680384110F11CL,
            -0x391FF40CC257703EL,
            -0x2A586EB86CF558DBL,
            0x6CA6351E003826FL,
            0x142929670A0E6E70L,
            0x27B70A8546D22FFCL,
            0x2E1B21385C26C926L,
            0x4D2C6DFC5AC42AEDL,
            0x53380D139D95B3DFL,
            0x650A73548BAF63DEL,
            0x766A0ABB3C77B2A8L,
            -0x7E3D36D1B812511AL,
            -0x6D8DD37AEB7DCAC5L,
            -0x5D40175EB30EFC9CL,
            -0x57E599B443BDCFFFL,
            -0x3DB4748F2F07686FL,
            -0x3893AE5CF9AB41D0L,
            -0x2E6D17E62910ADE8L,
            -0x2966F9DBAA9A56F0L,
            -0xBF1CA7AA88EDFD6L,
            0x106AA07032BBD1B8L,
            0x19A4C116B8D2D0C8L,
            0x1E376C085141AB53L,
            0x2748774CDF8EEB99L,
            0x34B0BCB5E19B48A8L,
            0x391C0CB3C5C95A63L,
            0x4ED8AA4AE3418ACBL,
            0x5B9CCA4F7763E373L,
            0x682E6FF3D6B2B8A3L,
            0x748F82EE5DEFB2FCL,
            0x78A5636F43172F60L,
            -0x7B3787EB5E0F548EL,
            -0x7338FDF7E59BC614L,
            -0x6F410005DC9CE1D8L,
            -0x5BAF9314217D4217L,
            -0x41065C084D3986EBL,
            -0x398E870D1C8DACD5L,
            -0x35D8C13115D99E64L,
            -0x2E794738DE3F3DF9L,
            -0x15258229321F14E2L,
            -0xA82B08011912E88L,
            0x6F067AA72176FBAL,
            0xA637DC5A2C898A6L,
            0x113F9804BEF90DAEL,
            0x1B710B35131C471BL,
            0x28DB77F523047D84L,
            0x32CAAB7B40C72493L,
            0x3C9EBE0A15C9BEBCL,
            0x431D67C49C100D4CL,
            0x4CC5D4BECB3E42B6L,
            0x597F299CFC657E2AL,
            0x5FCB6FAB3AD6FAECL,
            0x6C44198C4A475817L
    )

    val padding = byteArrayOf(
            0x80.toByte(), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
    )
  }

  private fun rotateRight(x: Long, distance: Int): Long {
    return (x ushr distance) or (x shl -distance)
  }

  private fun S0(x: Long): Long {
    return rotateRight(x, 1) xor rotateRight(x, 8) xor (x ushr 7)
  }

  private fun S1(x: Long): Long {
    return rotateRight(x, 19) xor rotateRight(x, 61) xor (x ushr 6)
  }

  private fun S2(x: Long): Long {
    return rotateRight(x, 28) xor rotateRight(x, 34) xor rotateRight(x, 39)
  }

  private fun S3(x: Long): Long {
    return rotateRight(x, 14) xor rotateRight(x, 18) xor rotateRight(x, 41)
  }

  private fun F0(x: Long, y: Long, z: Long): Long {
    return ((x and y) or (z and (x or y)))
  }

  private fun F1(x: Long, y: Long, z: Long): Long {
    return (z xor (x and (y xor z)))
  }

  private fun process(data: ByteArray, start: Int) {
    val W = arrayOf<Long>(0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L)
    for (k in 0 until 16)
      W[k] = getUInt64BE(data, start + (k shl 3))
    for (j in 16 until 80)
      W[j] = S1(W[j - 2]) + W[j - 7] + S0(W[j - 15]) + W[j - 16]

    var A = state[0]
    var B = state[1]
    var C = state[2]
    var D = state[3]
    var E = state[4]
    var F = state[5]
    var G = state[6]
    var H = state[7]

    var i: Int = 0
    do {
      var temp1: Long = H + S3(E) + F1(E, F, G) + K[i] + W[i]
      var temp2: Long = S2(A) + F0(A, B, C)
      D += temp1
      H = temp1 + temp2
      i++

      temp1 = G + S3(D) + F1(D, E, F) + K[i] + W[i]
      temp2 = S2(H) + F0(H, A, B)
      C += temp1
      G = temp1 + temp2
      i++

      temp1 = F + S3(C) + F1(C, D, E) + K[i] + W[i]
      temp2 = S2(G) + F0(G, H, A)
      B += temp1
      F = temp1 + temp2
      i++

      temp1 = E + S3(B) + F1(B, C, D) + K[i] + W[i]
      temp2 = S2(F) + F0(F, G, H)
      A += temp1
      E = temp1 + temp2
      i++

      temp1 = D + S3(A) + F1(A, B, C) + K[i] + W[i]
      temp2 = S2(E) + F0(E, F, G)
      H += temp1
      D = temp1 + temp2
      i++

      temp1 = C + S3(H) + F1(H, A, B) + K[i] + W[i]
      temp2 = S2(D) + F0(D, E, F)
      G += temp1
      C = temp1 + temp2
      i++

      temp1 = B + S3(G) + F1(G, H, A) + K[i] + W[i]
      temp2 = S2(C) + F0(C, D, E)
      F += temp1
      B = temp1 + temp2
      i++

      temp1 = A + S3(F) + F1(F, G, H) + K[i] + W[i]
      temp2 = S2(B) + F0(B, C, D)
      E += temp1
      A = temp1 + temp2
      i++
    } while (i < 80)

    state[0] = state[0] + A
    state[1] = state[1] + B
    state[2] = state[2] + C
    state[3] = state[3] + D
    state[4] = state[4] + E
    state[5] = state[5] + F
    state[6] = state[6] + G
    state[7] = state[7] + H
  }

  /*
   * SHA-512 process buffer
   */
  fun update(input: ByteArray, ilen0: Int) {
    var ilen = ilen0
    if (ilen == 0)
      return

    var left = total[0].toInt() and 0x7f
    val fill = 128 - left

    total[0] = total[0] + ilen.toLong()
    if (total[0] < ilen.toLong())
      total[1]++

    var inputIndex: Int = 0

    if (left != 0 && ilen >= fill) {
      //System.arraycopy(input, inputIndex, buffer, left, fill)
      for (k in 0 until fill)
        buffer[left + k] = input[inputIndex + k]
      process(buffer, 0)
      inputIndex += fill
      ilen -= fill
      left = 0
    }

    while (ilen >= 128) {
      process(input, inputIndex)
      inputIndex += 128
      ilen -= 128
    }

    if (ilen > 0) {
      //System.arraycopy(input, inputIndex, buffer, left, ilen)
      for (l in 0 until ilen)
        buffer[left + l] = input[inputIndex + l]
    }
  }

  /*
   * SHA-512 final digest
   */
  fun finish(output: ByteArray) {
    val high = (total[0] ushr 61) or (total[1] shl 3)
    val low = total[0] shl 3

    val msglen = byteArrayOf(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    putUInt64BE(msglen, 0, high)
    putUInt64BE(msglen, 8, low)

    val last = total[0].toInt() and 0x7f
    val padn = if (last < 112) (112 - last) else (240 - last)

    update(padding, padn)
    update(msglen, 16)

    putUInt64BE(output, 0, state[0])
    putUInt64BE(output, 8, state[1])
    putUInt64BE(output, 16, state[2])
    putUInt64BE(output, 24, state[3])
    putUInt64BE(output, 32, state[4])
    putUInt64BE(output, 40, state[5])

    if (!is384) {
      putUInt64BE(output, 48, state[6])
      putUInt64BE(output, 56, state[7])
    }
  }

}