var path = require("path");

const API_URL = process.env.API_URL || "127.0.0.1:8080";

module.exports = {
  // If your entry-point is at "src/index.js" and
  // your output is in "/dist", you can ommit
  // these parts of the config
  module: {
    rules: [
      {
        test: /\.html$/,
        exclude: /node_modules/,
        loader: "file-loader",
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: "elm-webpack-loader",
        options: {
          debug: false,
        },
      },
      {
        test: /\.css$/i,
        use: ["style-loader", "css-loader"],
      },
      {
        test: /\.(woff(2)?|ttf|eot|svg|ico)(\?v=\d+\.\d+\.\d+)?$/,
        use: [
          {
            loader: "file-loader",
          },
        ],
      },
    ],
  },

  devServer: {
    contentBase: path.join(__dirname, "src"),
    historyApiFallback: {
      index: "index.html",
    },
    stats: "errors-only",
    proxy: {
      "/api": {
        target: API_URL,
        pathRewrite: { "^/api": "" },
        logLevel: "debug",
      },
    },
  },
};
