import {storeCredentials, checkCredentials, Coder} from "../../src/helpers/Crypto";
import * as LS from "localstorage-memory";
(<any>window).localStorage = LS;

describe("Testing crypto.ts", () => {
  it("Coder.encrypt", async () => {
    const rawKey = [-1109790771, 1597817371, 586562914, 1041970040];
    const challenge = [206, 188, 95, 148, 105, 144, 143, 127, 229, 229, 72, 139, 18, 59, 133, 68, 90, 220, 118, 61, 216, 134, 193, 252, 5, 10, 187, 93, 127, 213, 40, 172];
    const nonce = [16, 211, 69, 35, 37, 229, 132, 150, 83, 254, 163, 241, 53, 44, 185, 67, 242, 230, 45, 144, 132, 235, 102, 89, 23, 60, 126, 77, 187, 152, 14, 129];
    const iv = [254, 255, 122, 107, 246, 190, 147, 191, 239, 255, 255, 188];
    const target = new Coder();
    const result = await target.encrypt(rawKey, challenge, nonce, iv);
    expect(result.iv).toEqual([254, 255, 122, 107, 246, 190, 147, 191, 239, 255, 255, 188]);
    expect(result.encrypted).toEqual([212, 8, 245, 234, 124, 24, 4, 112, 55, 91, 133, 99, 43, 136, 31, 111, 207, 39, 32, 153, 157, 66, 28, 238, 102, 90, 39, 135, 8, 101, 160, 71, 73, 151, 192, 147, 121, 36, 201, 132, 177, 212, 67, 107, 28, 126, 190, 150, 184, 221, 249, 253, 134, 201, 132, 143, 50, 4, 102, 107, 20, 67, 167, 129]);
    expect(result.authTag).toEqual([211, 24, 56, 236, 206, 253, 197, 99, 206, 35, 231, 227, 158, 224, 57, 122]);
  });

  it("checkCredentials", async () => {
    await storeCredentials("foo", "bar");
    expect(checkCredentials("foo", "bar")).resolves.toBe(expect.anything());
    expect(checkCredentials("foo", "bar2")).rejects.toBe(expect.anything());
    expect(checkCredentials("foo2", "bar")).rejects.toBe(expect.anything());
  });
});
