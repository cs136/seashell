import * as sjcl from "sjcl";
export {Coder}
class Coder {
  private cipher;

  constructor(key) {
    this.cipher = new sjcl.cipher.aes(key);
  }

  public encrypt(frame, plain) {
    var iv = sjcl.random.randomWords(12); // We'll generate 48 bytes of entropy and use 12.
    var ivArr = this.toBits(iv);
    var frameArr = this.toBits(frame);
    var plainArr = this.toBits(plain);
    var authArr = sjcl.bitArray.concat(ivArr, plainArr);
    var out = sjcl.mode.gcm.encrypt(this.cipher,
        frameArr,
        ivArr,
        authArr,
        128);
    var tagArr = sjcl.bitArray.bitSlice(out, sjcl.bitArray.bitLength(out) - 128);
    var codedArr = sjcl.bitArray.bitSlice(out, 0, sjcl.bitArray.bitLength(out) - 128);

    return [this.fromBits(ivArr), this.fromBits(codedArr), this.fromBits(tagArr)];
  }

  private toBits(bytes) {
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

  private fromBits(arr) {
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
}

/*
export interface CoderEncrypted {
  iv: ArrayBuffer;
  tag: ArrayBuffer;
  encrypted: ArrayBuffer;
}
export class Coder {
  key: CryptoKey;
  rawKey: Uint8Array;
  constructor(rawKey: Uint8Array) {
    this.rawKey = rawKey;
  }
  init = async () => {
    let result = await window.crypto.subtle.importKey(
      "raw",
      this.rawKey,
      { name: "AES-GCM" },
      false,
      ["encrypt", "decrypt"])
      .then((key) => {
        this.key = key;
        return true;
      });
    return result;
  }
  encrypt = async (data: ArrayBuffer) : Promise<CoderEncrypted> => {
    let iv = new Uint8Array(12);
    window.crypto.getRandomValues(iv);
    let encrypted_tag = await window.crypto.subtle.encrypt({
      name: "AES-GCM",
      iv: iv,
      additionalData: iv,
      tagLength: 128
    }, this.key, data);
    let encrypted = encrypted_tag.slice(0, encrypted_tag.byteLength - 8);
    let tag = encrypted_tag.slice(encrypted_tag.byteLength - 8);
    return {
      iv: new Uint8Array(iv).buffer,
      encrypted: new Uint8Array(encrypted).buffer,
      tag: new Uint8Array(tag).buffer,
    };
  }
  decrypt = async (encryptionResult: CoderEncrypted) : Promise<ArrayBuffer> => {
    let iv = encryptionResult.iv;
    let encrypted = encryptionResult.encrypted;
    let tag = encryptionResult.tag;
    let data = new Uint8Array(encrypted.byteLength + tag.byteLength);
    data.set(new Uint8Array(encrypted), 0);
    data.set(new Uint8Array(tag), encrypted.byteLength);
    let result = await window.crypto.subtle.decrypt({
      name: "AES-GCM",
      iv: iv,
      additionalData: iv,
      tagLength: 128,
    }, this.key, data);
    return result;
  }
}
//*/