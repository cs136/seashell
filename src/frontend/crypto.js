/**
 * Seashell's cryptographic communications backend.
 * Copyright (C) 2013 The Seashell Maintainers.
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

/** Adds entropy to the random pool that we use to generate IV's.
 * @static
 * @param {String|Array} entropy - Entropy to add.
 */
SeashellCoder.addEntropy = function(entropy) {
  sjcl.random.addEntropy(entropy);
}

/** Encrypts an array of bytes.
 * @param {Array} frame - frame of bytes to encrypt.
 * @param {Array} plain - extra authenticated data.
 * @returns {Array} [IV, coded, tag], all arrays of bytes.  IV must
 *  be 12 bytes, and tag must be 16 bytes.
 */
SeashellCoder.prototype.encrypt = function(frame, plain) {
  var iv = sjcl.random.randomWords(3);
  var ivArr = sjcl.codec.bytes.toBits(iv);
  var frameArr = sjcl.codec.bytes.toBits(frame);
  var plainArr = sjcl.codec.bytes.toBits(plain);
  var authArr = sjcl.codec.bytes.concat(ivArr, plainArr);
  var out = sjcl.mode.gcm.encrypt(this.cipher,
      frameArr,
      ivArr,
      plainArr,
      128);
  var tagArr = sjcl.bitArray.bitSlice(out, out.bitLength() - 128);
  var codedArr = sjcl.bitArray.bitSlice(out, 0, out.bitLength() - 128);

  return [iv, sjcl.codec.bytes.fromBits(codedArr), sjcl.codec.bytes.fromBits(tagArr)];
};

/** Decrypts an array of bytes and verifies the extra authenticated data.
 * @param {Array} coded - encrypted frame.
 * @param {Array} IV - IV used.
 * @param {Array} tag - GCM verification tag.
 * @param {Array} plain - Extra authenticated data.
 * @returns {Array} Decrypted data, or will throw an exception.  Consult SJCL
 * documentation. */
SeashellCoder.prototype.decrypt = function(coded, iv, tag, plain) {
  var ivArr = sjcl.codec.bytes.toBits(iv);
  var plainArr = sjcl.codec.bytes.toBits(plain);
  var authArr = sjcl.bitArray.concat(ivArr, plainArr);
  var codedArr = sjcl.codec.bytes.toBits(coded);
  var tagArr = sjcl.codec.bytes.toBits(tag);
  var cryptedArr = sjcl.bitArray.concat(codedArr, tagArr);
  var result = sjcl.mode.gcm.decrypt(this.cipher,
      cryptedArr,
      ivArr,
      authArr,
      128);
  return sjcl.codec.bytes.fromBits(result);
};
