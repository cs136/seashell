"use strict";
/**
 * Seashell's cryptographic communications backend.
 * Copyright (C) 2013-2015 The Seashell Maintainers.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * See also 'ADDITIONAL TERMS' at the end of the included LICENSE file.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/** Make sure we start the entropy collectors before doing anything stupid. */
sjcl.random.startCollectors();


/** Seashell's Encryption/Decryption class.
 * @constructor
 * @param {Array} key - Array of 4 words that represent the 128-bit AES key.
 *
 * Implementor's note - this class must maintain consistent with the
 * code written in crypto.rkt [Racket FFI] and crypto.c [C Implementation].
 */
function SeashellCoder(key) {
  this.cipher = new sjcl.cipher.aes(key);
}

/** Helper functions for dealing with bytes.  Taken from SJCL. */
SeashellCoder.Bytes = {
  /** Convert from a bitArray to an array of bytes. */
  fromBits: function (arr) {
    var out = [], bl = sjcl.bitArray.bitLength(arr), i, tmp;
    for (i=0; i<bl/8; i++) {
      if ((i&3) === 0) {
        tmp = arr[i/4];
      }
    out.push(tmp >>> 24);
    tmp <<= 8;
  }
  return out;
  },
  /** Convert from an array of bytes to a bitArray. */
  toBits: function (bytes) {
    var out = [], i, tmp=0;
    for (i=0; i<bytes.length; i++) {
      tmp = tmp << 8 | bytes[i];
      if ((i&3) === 3) {
      out.push(tmp);
      tmp = 0;
      }
    }
    if (i&3) {
      out.push(sjcl.bitArray.partial(8*(i&3), tmp));
    }
    return out;
  }
};

/** Adds entropy to the random pool that we use to generate IV's.
 * @static
 * @param {String|Array} entropy - Entropy to add.  If null, adds
 *        entropy generated from window.crypto.getRandomValues()
 */
SeashellCoder.addEntropy = function(entropy) {
  if (!entropy) {
    entropy = new Uint32Array(32);

    if (window.crypto)
      window.crypto.getRandomValues(entropy);
    else if (window.msCrypto)
      window.msCrypto.getRandomValues(entropy);
  }
  sjcl.random.addEntropy(entropy);
};

/** Encrypts an array of bytes.
 * @param {Array} frame - frame of bytes to encrypt.
 * @param {Array} plain - extra authenticated data.
 * @returns {Array} [IV, coded, tag], all arrays of bytes.  IV must
 *  be 12 bytes, and tag must be 16 bytes.
 */
SeashellCoder.prototype.encrypt = function(frame, plain) {
  var iv = sjcl.random.randomWords(12); // We'll generate 48 bytes of entropy and use 12.
  var ivArr = SeashellCoder.Bytes.toBits(iv);
  var frameArr = SeashellCoder.Bytes.toBits(frame);
  var plainArr = SeashellCoder.Bytes.toBits(plain);
  var authArr = sjcl.bitArray.concat(ivArr, plainArr);
  var out = sjcl.mode.gcm.encrypt(this.cipher,
      frameArr,
      ivArr,
      authArr,
      128);
  var tagArr = sjcl.bitArray.bitSlice(out, sjcl.bitArray.bitLength(out) - 128);
  var codedArr = sjcl.bitArray.bitSlice(out, 0, sjcl.bitArray.bitLength(out) - 128);

  return [SeashellCoder.Bytes.fromBits(ivArr), SeashellCoder.Bytes.fromBits(codedArr), SeashellCoder.Bytes.fromBits(tagArr)];
};

/** Decrypts an array of bytes and verifies the extra authenticated data.
 * @param {Array} coded - encrypted frame.
 * @param {Array} IV - IV used.
 * @param {Array} tag - GCM verification tag.
 * @param {Array} plain - Extra authenticated data.
 * @returns {Array} Decrypted data, or will throw an exception.  Consult SJCL
 * documentation. */
SeashellCoder.prototype.decrypt = function(coded, iv, tag, plain) {
  var ivArr = SeashellCoder.Bytes.toBits(iv);
  var plainArr = SeashellCoder.Bytes.toBits(plain);
  var authArr = sjcl.bitArray.concat(ivArr, plainArr);
  var codedArr = SeashellCoder.Bytes.toBits(coded);
  var tagArr = SeashellCoder.Bytes.toBits(tag);
  var cryptedArr = sjcl.bitArray.concat(codedArr, tagArr);
  var result = sjcl.mode.gcm.decrypt(this.cipher,
      cryptedArr,
      ivArr,
      authArr,
      128);
  return SeashellCoder.Bytes.fromBits(result);
};
