const path = require('path');
const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');
module.exports = {
  entry: [
    './src/index.tsx'
  ],
  output: {
    path: path.join(__dirname, 'dist'),
    filename: 'bundle.[chunkhash].js',
    publicPath: '/',
  },
  plugins: [
    new CopyWebpackPlugin([
      { from: 'images/', to: 'images/' },
      {
        from: 'favicon.ico'
      },
      { from: 'manifest.json' },
      {
        from: './node_modules/monaco-editor/min/vs',
        to: 'vs',
      },
      { context: './node_modules/seashell-clang-js/bin/',
        from: '*.mem', to: './' },
      { context: './node_modules/seashell-clang-js/bin/',
        from: '*.data', to: './' },
    ]),
    new webpack.DefinePlugin({
      IS_BROWSER: true,
      PRODUCTION: false,
      VERSION: "'local'"
    }),
    new HtmlWebpackPlugin ({
      inject: true,
      template: './src/index.html'
    }),
  ],
  devtool: "source-map",
  resolve: {
      // Add '.ts' and '.tsx' as resolvable extensions.
      extensions: [".webpack.js", ".web.js", ".ts", ".tsx", ".js", ".scss"]
  },
  module: {
    rules: [
      { enforce: 'pre', test: /\.js$/, loader: "source-map-loader" },
      { enforce: 'pre', test: /\.tsx?$/, loader: "tslint-loader" },
      { test: /\.tsx?$/, loader: "awesome-typescript-loader" },
      { test: /\.scss$/,
        use: [
          'style-loader?sourceMap',
          'css-loader?modules&importLoaders=1&localIdentName=[path]___[name]__[local]___[hash:base64:5]',
          'sass-loader?sourceMap'
        ]
      }, {
        test: /\.css$/,
        loader: "style-loader!css-loader"
      }, {
        test: /\.(woff|woff2|ttf|eot)$/,
        loader: 'file-loader'
      }, {
        test: /\.(jpe?g|png|gif|svg)$/i,
        use: [
          'file-loader?hash=sha512&digest=hex&name=[hash].[ext]',
          'image-webpack-loader?{bypassOnDebug: true, optipng: {optimizationLevel: 7}, gifsicle: {interlaced: false}}'
        ]
      }
    ]
  },
};
