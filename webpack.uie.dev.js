const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");

const API_URL = process.env.API_URL || "127.0.0.1:8080";

module.exports = {
  entry: "./src/UIE/uie.js",

  module: {
    rules: [
      {
        test: /\.css$/i,
        use: ["style-loader", "css-loader"],
      },
      {
        test: /\.(png|svg|jpg|jpeg|gif)$/i,
        type: "asset/resource",
      },
      {
        test: /\.(woff(2)?|ttf|eot)$/i,
        type: "asset/resource",
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          {
            loader: "elm-asset-webpack-loader",
          },
          {
            loader: "elm-webpack-loader",
            options: {
              debug: false,
              cwd: __dirname,
            },
          },
        ],
      },
    ],
  },

  plugins: [
    new HtmlWebpackPlugin({
      favicon: "./static/favicon.ico",
      template: "./src/UIE/index.html",
      inject: "body",
      publicPath: "/",
      base: "/",
      filename: path.resolve(__dirname, "dist/dev/index.html"),
    }),
  ],

  output: {
    filename: "[name].[contenthash].js",
    path: path.resolve(__dirname, "dist/dev"),
    clean: true,
  },

  devServer: {
    historyApiFallback: {
      disableDotRule: true,
    },
  },
};
