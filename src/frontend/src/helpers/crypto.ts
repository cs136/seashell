import WebCrypto = require("node-webcrypto-ossl");
import CryptoKey = require("node-webcrypto-ossl");
import sjcl = require("sjcl");
export {Coder, CoderEncrypted, AuthKey}

const webcrypto = new WebCrypto();
type AuthKey = Uint8Array;

interface CoderEncrypted {
  iv: Uint8Array;
  tag: Uint8Array;
  encrypted: Uint8Array;
}

class Coder {
  key: NodeWebcryptoOpenSSL.CryptoKey;

  constructor(public rawKey?: number[]) {};

  public async encrypt(rawKey: number[], challenge: number[], nonce: number[], iv: number[]): Promise<{
      iv: number[],
      encrypted: number[],
      authTag: number[],
    }> {
    console.log(`rawKey[${rawKey.length}]: ${rawKey}`);
    console.log(`Server challenge[${challenge.length}]: ${challenge}`);
    console.log(`client_nonce[${nonce.length}] ${nonce}`);
    console.log(`iv[${iv.length}] ${iv}`);
    // let iv = new Uint8Array(48);

    const raw_response = new Uint8Array(nonce.length + challenge.length);
    raw_response.set(nonce, 0);
    raw_response.set(challenge, nonce.length);
    console.log(`raw_response[${raw_response.length}] ${raw_response}`);

    const key8 = new Int8Array(Int32Array.from(rawKey).buffer);
    const key = await webcrypto.subtle.importKey("raw", key8, { name: "AES-GCM" }, false, ["encrypt", "decrypt"]);

    let result = await webcrypto.subtle.encrypt({
      name: "AES-GCM",
      iv: new Int32Array(iv),
      additionalData: new Int32Array(iv),
      tagLength: 128
    }, key, raw_response);

    console.warn(`result[${result.byteLength}] ${new Uint8Array(result)}`);
    let encrypted = result.slice(0, result.byteLength - 16);
    console.warn(`encrypted[${encrypted.byteLength}] ${new Uint8Array(encrypted)}`);
    let authTag = result.slice(result.byteLength - 16);
    console.warn(`authTag[${authTag.byteLength}] ${new Uint8Array(authTag)}`);

    return {
      iv: iv,
      encrypted: Array.from(new Uint8Array(encrypted)),
      authTag: Array.from(new Uint8Array(authTag)),
    };
  }

  public async shittyEncrypt(rawKey: number[], server_challenge: number[], client_nonce: number[], iv: number[]): Promise<{
      iv: number[],
      encrypted: number[],
      authTag: number[],
    }> {
    var plain = [];
    var cipher = new sjcl.cipher.aes(rawKey);
    /** OK, now we proceed to authenticate. */
    var raw_response = [].concat(client_nonce, server_challenge);

    // console.warn("server_challenge",server_challenge);
    // console.warn("client_nonce",client_nonce);
    // console.warn("raw_response",raw_response);

    var ivArr = this.toBits(iv);

    // console.warn("iv", iv);
    // console.warn("ivArr", ivArr);
    // console.warn("this.fromBits(ivArr)", this.fromBits(ivArr));

    var frameArr = this.toBits(raw_response);
    var plainArr = this.toBits(plain);
    var authArr = sjcl.bitArray.concat(ivArr, plainArr);
    var out = sjcl.mode.gcm.encrypt(cipher,
        frameArr,
        ivArr,
        authArr,
        128);
    var tagArr = sjcl.bitArray.bitSlice(out, sjcl.bitArray.bitLength(out) - 128);
    var codedArr = sjcl.bitArray.bitSlice(out, 0, sjcl.bitArray.bitLength(out) - 128);

    // console.warn("out", out);
    // console.warn("this.fromBits(ivArr)", this.fromBits(ivArr));
    // console.warn("this.fromBits(codedArr)", this.fromBits(codedArr));
    // console.warn("this.fromBits(tagArr)", this.fromBits(tagArr));

    return {
      iv: this.fromBits(ivArr),
      encrypted: this.fromBits(codedArr),
      authTag: this.fromBits(tagArr),
    };
  }

  public async shittyAnswer(server_challenge: number[]): Promise<{
      iv: number[],
      encrypted: number[],
      authTag: number[],
      nonce: number[]
    }> {

    var iv = sjcl.random.randomWords(12); // We'll generate 48 bytes of entropy and use 12.
    var client_nonce = sjcl.random.randomWords(32);
    for (var i = 0; i < client_nonce.length; i++) {
      client_nonce[i] = client_nonce[i] & 0xFF;
    }

    /** OK, now we proceed to authenticate. */
    const result = await this.shittyEncrypt(this.rawKey, server_challenge, client_nonce, iv); 
    return {
      iv: result.iv,
      encrypted: result.encrypted,
      authTag: result.authTag,
      nonce: client_nonce
    }
  }

  public async answer(server_challenge: number[]): Promise<{
      iv: number[],
      encrypted: number[],
      authTag: number[],
      nonce: number[]
    }> {
    const iv = new Int32Array(32);
    webcrypto.getRandomValues(iv);    
    /** Generate a nonce. */
    const client_nonce = new Uint8Array(32);
    new WebCrypto().getRandomValues(client_nonce);

    const arr_iv = Array.from(iv);
    const arr_client_nonce = Array.from(client_nonce);
    for (var i = 0; i < client_nonce.length; i++) {
      client_nonce[i] = client_nonce[i] & 0xFF;
    }
    /** OK, now we proceed to authenticate. */
    const result = await this.encrypt(this.rawKey, server_challenge, arr_client_nonce, arr_iv); 
    return {
      iv: result.iv,
      encrypted: result.encrypted,
      authTag: result.authTag,
      nonce: arr_client_nonce
    }
  }

  public async decrypt(encryptionResult: CoderEncrypted): Promise<ArrayBuffer> {
    let iv = encryptionResult.iv;
    let encrypted = encryptionResult.encrypted;
    let tag = encryptionResult.tag;
    let data = new Uint8Array(encrypted.byteLength + tag.byteLength);
    data.set(new Uint8Array(encrypted), 0);
    data.set(new Uint8Array(tag), encrypted.byteLength);
    let result = await webcrypto.subtle.decrypt({
      name: "AES-GCM",
      iv: iv,
      additionalData: iv,
      tagLength: 128,
    }, this.key, data);
    return result;
  }

  /** Convert from a bitArray to an array of bytes. */
  public fromBits(arr: number[]) {
    var out = [], bl = sjcl.bitArray.bitLength(arr), i, tmp;
    for (i=0; i<bl/8; i++) {
      if ((i&3) === 0) {
        tmp = arr[i/4];
      }
      out.push(tmp >>> 24);
      tmp <<= 8;
    }
    return out;
  }
  /** Convert from an array of bytes to a bitArray. */
  public toBits(bytes: number[]) {
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
}
//*/