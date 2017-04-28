import {mergeWith, clone, type} from "ramda";
export function mergeBetter<U>(a: U, b: any): U {
  let result = (typeof a === "object" && typeof b === "object" && a && b) ?
    mergeWith<U, any>(mergeBetter, a, b) : <U>b;
  if (typeof a === "object" && result) {
    // Repair result prototype
    Object.setPrototypeOf(result, Object.getPrototypeOf(a));
  }
  return result;
}
function _cloneBetter<U>(value: U, refFrom: any[], refTo: any[], deep: boolean): U {
  let copy = function copy(copiedValue: any) {
    let len = refFrom.length;
    let idx = 0;
    while (idx < len) {
      if (value === refFrom[idx]) {
        return refTo[idx];
      }
      idx += 1;
    }
    refFrom[idx + 1] = value;
    refTo[idx + 1] = copiedValue;
    for (let key in value) {
      copiedValue[key] = deep ?
        _cloneBetter(value[key], refFrom, refTo, true) : value[key];
    }
    return copiedValue;
  };
  switch (type(value)) {
    case "Object":  return typeof (<any>value).clone === "function" ? (<any>value).clone() : copy({});
    case "Array":   return copy([]);
    case "Date":    return <U><any>new Date(<Date><any>value.valueOf());
    case "RegExp":  return clone(value);
    default:        return value;
  }
};
export function cloneBetter<U>(value: U) {
  return _cloneBetter<U>(value, [], [], true);
}

export const groupBy = (lst: any[], grp: (a: any) => string): {[key: string]: any} => {
  const hash = lst.reduce((acc: any, item: any) => {
    const k = grp(item);
    if (!acc[k]) acc[k] = [];
    acc[k].push(item);
    return acc;
  }, []);
  let result = [];
  for (const k in hash) {
    result.push(hash[k]);
  }
  return result;
};
