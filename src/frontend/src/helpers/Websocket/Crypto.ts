import sjcl = require("sjcl");
export {Coder, CoderEncrypted, ShittyCoder}

const win: any = window as any, webcrypto = (win.crypto || win.msCrypto);


interface CoderEncrypted {
  iv: number[];
  authTag: number[];
  encrypted: number[];
  nonce: number[]
}

// factored some code out in the abstract class
// so that you can migrate away from the shitty sjcl later if every possible
abstract class AbstractCoder {
  constructor(public rawKey?: number[]) {};
  public abstract async encrypt(rawKey: number[], challenge: number[], nonce: number[], iv: number[]): Promise<CoderEncrypted>;
  public abstract async genRandom(): Promise<{iv: number[], nonce: number[]}>;

  public async answer(challenge: number[]): Promise<CoderEncrypted> {
      const rand = await this.genRandom();
      const result = await this.encrypt(this.rawKey, challenge, rand.nonce, rand.iv);
    return {
      iv: result.iv,
      encrypted: result.encrypted,
      authTag: result.authTag,
      nonce: result.nonce
    }
  }
}

// new but doesn't work
class Coder extends AbstractCoder {

  key: NodeWebcryptoOpenSSL.CryptoKey;

  constructor(public rawKey?: number[]) {
    super();
  };

  public async genRandom(): Promise<{ iv: number[], nonce: number[]} > {
    const iv = new Int32Array(32);
    webcrypto.getRandomValues(iv);
    const client_nonce = new Uint8Array(32);
    new webcrypto.getRandomValues(client_nonce);
    return {
      iv: Array.from(iv),
      nonce: Array.from(client_nonce)
    }
  };

  public async encrypt(rawKey: number[], challenge: number[], nonce: number[], iv: number[]): Promise<CoderEncrypted> {

    const raw_response = new Uint8Array(nonce.length + challenge.length);
    raw_response.set(nonce, 0);
    raw_response.set(challenge, nonce.length);

    const key8 = new Int8Array(Int32Array.from(rawKey).buffer);
    const key = await webcrypto.subtle.importKey("raw", key8, { name: "AES-GCM" }, false, ["encrypt", "decrypt"]);

    let result = await webcrypto.subtle.encrypt({
      name: "AES-GCM",
      iv: new Int32Array(iv),
      additionalData: new Int32Array(iv),
      tagLength: 128
    }, key, raw_response);

    let encrypted = result.slice(0, result.byteLength - 16);
    let authTag = result.slice(result.byteLength - 16);

    return {
      iv: iv,
      encrypted: Array.from(new Uint8Array(encrypted)),
      authTag: Array.from(new Uint8Array(authTag)),
      nonce: nonce
    };
  }


  public async decrypt(encryptionResult: CoderEncrypted): Promise<ArrayBuffer> {
    let iv = encryptionResult.iv;
    let encrypted = encryptionResult.encrypted;
    let tag = encryptionResult.authTag;
    let data = new Uint8Array(encrypted.length + tag.length);
    data.set(new Uint8Array(encrypted), 0);
    data.set(new Uint8Array(tag), encrypted.length);
    let result = await webcrypto.subtle.decrypt({
      name: "AES-GCM",
      iv: iv,
      additionalData: iv,
      tagLength: 128,
    }, this.key, data);
    return result;
  }

}

// shitty but works
class ShittyCoder extends AbstractCoder {

  key: NodeWebcryptoOpenSSL.CryptoKey;

  constructor(public rawKey?: number[]) {
    super(rawKey);
  };

  public async encrypt(rawKey: number[],
                       server_challenge: number[],
                       client_nonce: number[],
                       iv: number[]): Promise<CoderEncrypted> {
    var cipher = new sjcl.cipher.aes(rawKey);
    /** OK, now we proceed to authenticate. */
    var raw_response = [].concat(client_nonce, server_challenge);

    var ivArr = this.toBits(iv);

    var frameArr = this.toBits(raw_response);
    var authArr = ivArr;
    var out = sjcl.mode.gcm.encrypt(cipher,
        frameArr,
        ivArr,
        authArr,
        128);
    var tagArr = sjcl.bitArray.bitSlice(out, sjcl.bitArray.bitLength(out) - 128, sjcl.bitArray.bitLength(out));
    var codedArr = sjcl.bitArray.bitSlice(out, 0, sjcl.bitArray.bitLength(out) - 128);

    return {
      iv: this.fromBits(ivArr),
      encrypted: this.fromBits(codedArr),
      authTag: this.fromBits(tagArr),
      nonce: client_nonce
    };
  }

  public async genRandom(): Promise<{iv: number[], nonce: number[]}> {
    var iv = sjcl.random.randomWords(12); // We'll generate 48 bytes of entropy and use 12.
    var client_nonce = sjcl.random.randomWords(32);
    for (var i = 0; i < client_nonce.length; i++) {
      client_nonce[i] = client_nonce[i] & 0xFF;
    }
    return {
      iv: iv,
      nonce: client_nonce
    }
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
