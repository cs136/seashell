export {AbstractCoder, Coder, CoderEncrypted, storeCredentials, checkCredentials}

let Buffer = require("buffer").Buffer;

const webcrypto = window.crypto ? window.crypto : <Crypto> (() => {
  if (!IS_BROWSER) {
    // tslint:disable-next-line
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

abstract class AbstractCoder {
  constructor(public rawKey: number[]) {};
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
  constructor(public rawKey: number[]) {
    super(rawKey);
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

async function hashCredentials(salt: Uint8Array, password: string) {
  let bytes = Buffer.from(password, "utf8");
  let rawKey = await webcrypto.subtle.importKey(
    "raw",
    bytes,
    {name: "PBKDF2"},
    false,
    ["deriveBits", "deriveKey"]);
  let key = await webcrypto.subtle.deriveKey({
    "name": "PBKDF2",
    "salt": salt,
    "iterations": 10000,
    "hash": "SHA-512"},
    rawKey,
    { "name": "AES-CBC", "length": 256},
    true,
    [ "encrypt", "decrypt"]);
 let result = await webcrypto.subtle.exportKey("raw", key);
 return Buffer.from(result).toString("hex");
}

async function storeCredentials(user: string, password: string) {
  let salt = new Uint8Array(12);
  webcrypto.getRandomValues(salt);

  let cached = await hashCredentials(salt, password);
  let toStore = JSON.stringify({salt: Buffer.from(salt).toString("hex"),
                                credentials: cached});
  localStorage.setItem("credentials-" + user, toStore);
}

async function checkCredentials(user: string, password: string) {
  let data = localStorage.getItem("credentials-" + user);
  if (!data)
    throw new Error("User " + user + " has no offline credentials!");

  let creds = JSON.parse(data);
  let expected = creds.credentials;
  let have = await hashCredentials(Buffer.from(creds.salt, "hex"), password);

  if (expected !== have) {
    throw new Error("User " + user + " credentials do not match saved credentials!");
  }

  return true;
}
