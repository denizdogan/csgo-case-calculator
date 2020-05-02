module.exports = {
  "*.{html,js,json,pcss}": ["prettier --write"],
  "*.{js}": ["eslint --fix"],
  "*.elm": ["elm-format --yes"],
};
