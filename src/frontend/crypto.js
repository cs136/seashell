function coder(key) {
  this.key = key;
}

coder.prototype.encrypt = function(frame, plain) {
/*
  var iv = sjcl.random.randomWords(3);
  var frameArr = sjcl.codec.bytes.toBits(frame);
  var plainArr = sjcl.codec.bytes.toBits(plain);
  var ivArr = sjcl.codec.bytes.toBits(iv);
  var out = sjcl.mode.gcm.encrypt(new sjcl.cipher.aes(this.key),
      frameArr,
      ivArr,
      plainArr,
      128);
  var tag = sjcl.bitArray.bitSlice(out, 0, 128);
  var enc = sjcl.bitArray.bitSlice(out, 128);
  return [iv, tag, enc]; */
}

coder.prototype.decrypt = function(coded, iv, tag, plain) {
  var ivArr = sjcl.codec.bytes.toBits(iv);
  var plainArr = sjcl.codec.bytes.toBits(plain);
  var authArr = sjcl.bitArray.concat(ivArr, plainArr);
  var codedArr = sjcl.codec.bytes.toBits(coded);
  var tagArr = sjcl.codec.bytes.toBits(tag);
  var cryptedArr = sjcl.bitArray.concat(codedArr, tagArr);
  return sjcl.mode.gcm.decrypt(new sjcl.cipher.aes(this.key),
      cryptedArr,
      ivArr, 
      authArr,
      128);
//  return sjcl.mode.gcm._ctrMode(false, new sjcl.cipher.aes(key), codedArr, authArr, ivArr, 128)
}
/** test case - see crypto.rkt.  Dummey key [255, 1, .... 16] 
var key = [-16645372,84281096,151653132,219025168];
var iv = [155,55,239,43,182,237,180,241,188,255,122,37];
var coded = [118,93,166,161,4,16,81,80,164,154,250,119,102,119,178,151,130,30,125,128,58,91,51,248,165,155,99,72,132,14,142,140,177,184,27,192,8,211,254,28,88,167,206,232,165,12,102,236,169,218,123,89,254,199,239,6,82,194,53,133,234,123,210,88,116,186,77,142,184,96,176,188,240,61,74,241,60,228,193,8,181,126,236,136,240,107,12,233,64,182,57,15,108,135,220,185,170,255,182,201,53,1,254,58,68,23,99,74];
var tag = [118,101,55,87,155,33,164,224,83,209,117,10,111,56,108,209];
var plain = [97,117,116,104,101,110,116,105,99,97,116,101,100];
var x = new coder(key);
x.decrypt(coded, iv, tag, plain); */
sjcl.random.startCollectors();
