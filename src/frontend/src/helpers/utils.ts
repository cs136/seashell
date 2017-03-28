import {mergeWith} from "ramda";
export const mergeBetter: any = (a: any, b: any) => (typeof a === "object" && typeof b === "object") ? mergeWith(mergeBetter, a, b) : b;
export const groupBy = (lst: any[], grp: (a: any) => any) => {
  const hash = lst.reduce((acc: any, item: any) => {
    const k = grp(item);
    if (!acc[k]) acc[k] = [];
    acc[k].push(item);
  }, []);
  let result = [];
  for (const k in hash) {
    result.push(hash[k]);
  }
  return result;
};
