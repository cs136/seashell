import sjcl = require("sjcl");
export {AbstractCoder, Coder, CoderEncrypted, ShittyCoder}

const webcrypto = window.crypto ? window.crypto : <Crypto> (() => {
  if (!IS_BROWSER) {
    const WebCrypto = eval("require")("node-webcrypto-ossl");
    return new WebCrypto();
  } else {
    throw new Error("WebCrypto not found!");
  }
})();


interface CoderEncrypted {
  iv: number[];
  authTag: number[];
  encrypted: number[];
  nonce: number[];
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
    };
  }
}

class Coder extends AbstractCoder {
  constructor(public rawKey?: number[]) {
    super();
  };

  public async genRandom(): Promise<{ iv: number[], nonce: number[]} > {
    const iv = new Uint8Array(12);
    webcrypto.getRandomValues(iv);
    const client_nonce = new Uint8Array(32);
    webcrypto.getRandomValues(client_nonce);
    return {
      iv: Array.from(iv),
      nonce: Array.from(client_nonce)
    };
  };

  public async encrypt(rawKey: number[], challenge: number[], nonce: number[], iv: number[]): Promise<CoderEncrypted> {
    const raw_response = new Uint8Array(nonce.length + challenge.length);
    raw_response.set(nonce, 0);
    raw_response.set(challenge, nonce.length);

    /** Convert rawKey to bytes using a DataView
     *  We do this as the key needs to be encoded big-endian.
     */
    let keyLength = rawKey.length * 4;
    let buffer = new ArrayBuffer(keyLength);
    let dataview = new DataView(buffer);
    for (let i = 0; i < rawKey.length; i++) {
      dataview.setInt32(i * 4, rawKey[i]);
    }
    let key8 = new Uint8Array(buffer);
    const key = await webcrypto.subtle.importKey("raw", key8,
      { name: "AES-GCM" }, false, ["encrypt", "decrypt"]);

    let result = await webcrypto.subtle.encrypt({
      name: "AES-GCM",
      iv: new Uint8Array(iv),
      additionalData: new Uint8Array(iv),
      tagLength: 128
    }, key, raw_response);

    let encrypted = result.slice(0, result.byteLength - 16);
    let authTag = result.slice(result.byteLength - 16);

    let ourResult = {
      iv: iv,
      encrypted: Array.from(new Uint8Array(encrypted)),
      authTag: Array.from(new Uint8Array(authTag)),
      nonce: nonce
    };
    return ourResult;
  }
}

class ShittyCoder extends AbstractCoder {
  constructor(public rawKey?: number[]) {
    super(rawKey);
  };

  public async encrypt(rawKey: number[],
                       server_challenge: number[],
                       client_nonce: number[],
                       iv: number[]): Promise<CoderEncrypted> {
    let cipher = new sjcl.cipher.aes(rawKey);
    /** OK, now we proceed to authenticate. */
    let raw_response = [].concat(client_nonce, server_challenge);

    let ivArr = this.toBits(iv);

    let frameArr = this.toBits(raw_response);
    let authArr = ivArr;
    let out = sjcl.mode.gcm.encrypt(cipher,
        frameArr,
        ivArr,
        authArr,
        128);
    let tagArr = sjcl.bitArray.bitSlice(out, sjcl.bitArray.bitLength(out) - 128, sjcl.bitArray.bitLength(out));
    let codedArr = sjcl.bitArray.bitSlice(out, 0, sjcl.bitArray.bitLength(out) - 128);

    return {
      iv: this.fromBits(ivArr),
      encrypted: this.fromBits(codedArr),
      authTag: this.fromBits(tagArr),
      nonce: client_nonce
    };
  }

  public async genRandom(): Promise<{iv: number[], nonce: number[]}> {
    let iv = sjcl.random.randomWords(12); // We'll generate 48 bytes of entropy and use 12.
    let client_nonce = sjcl.random.randomWords(32);
    for (let i = 0; i < client_nonce.length; i++) {
      client_nonce[i] = client_nonce[i] & 0xFF;
    }
    return {
      iv: iv,
      nonce: client_nonce
    };
  }

  /** Convert from a bitArray to an array of bytes. */
  public fromBits(arr: number[]) {
    let out = [], bl = sjcl.bitArray.bitLength(arr), i, tmp;
    for (i = 0; i < bl / 8; i++) {
      if ((i & 3) === 0) {
        tmp = arr[i / 4];
      }
      out.push(tmp >>> 24);
      tmp <<= 8;
    }
    return out;
  }
  /** Convert from an array of bytes to a bitArray. */
  public toBits(bytes: number[]) {
    let out = [], i, tmp = 0;
    for (i = 0; i < bytes.length; i++) {
      tmp = tmp << 8 | bytes[i];
      if ((i & 3) === 3) {
      out.push(tmp);
      tmp = 0;
      }
    }
    if (i & 3) {
      out.push(sjcl.bitArray.partial(8 * ( i & 3), tmp));
    }
    return out;
  }

}
