'use strict';

const unsafeParseInt10 = (str) => {
  return parseInt(str, 10);
};

const unsafeParseIntBase = (str) => {
  return function(base) {
    return parseInt(str, base);
  };
};

const unsafeParseFloat = (str) => {
  return parseFloat(str);
};

const getInputDirectory = () => {
  return process.env.AOC_INPUT_DIRECTORY;
};

export { unsafeParseInt10, unsafeParseIntBase, unsafeParseFloat, getInputDirectory }
