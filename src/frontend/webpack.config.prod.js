const path = require('path');
const webpack = require('webpack');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const CleanWebpackPlugin = require('clean-webpack-plugin');
const SWPrecacheWebpackPlugin = require('sw-precache-webpack-plugin');
const OfflinePlugin = require('offline-plugin');
const ArchivePlugin = require('webpack-archive-plugin');

module.exports = {
  entry: './src/index.tsx',
  output: {
    path: path.join(__dirname, 'dist'),
    filename: 'bundle.js',
    publicPath: '/dist/'
  },
  plugins: [
    new CleanWebpackPlugin(['dist'], { verbose: false }),
    new CopyWebpackPlugin([
      { from: 'images/', to: 'images/' },
      { from: 'manifest.json' }]),
    new webpack.optimize.UglifyJsPlugin({
      compress: {
        warnings: false,
      },
      sourceMap: false,
    }),
    new webpack.DefinePlugin({
      'process.env.NODE_ENV': JSON.stringify('production'),
    }),
    new SWPrecacheWebpackPlugin(
      {
        cacheId: 'uwseashell-pwa',
        filename: 'uwseashell-service-worker.js',
        runtimeCaching: [{
          handler: 'cacheFirst',
          urlPattern: /cardstack_search$/,
        },
        {
          handler: 'cacheFirst',
          urlPattern: /[.]jpg$/,
        }],
      }
    ),
    new OfflinePlugin({
      ServiceWorker:{
        navigateFallbackURL: '/'
      }
    }),
    new ArchivePlugin()
  ],
  resolve: {
      // Add '.ts' and '.tsx' as resolvable extensions.
      extensions: ["", ".webpack.js", ".web.js", ".ts", ".tsx", ".js", ".scss"]
  },
  module: {
    preLoaders: [
      { test: /\.js$/, loader: "source-map-loader" },
      { test: /\.json$/, loader: 'json'},
      { test: /\.tsx?$/, loader: "tslint-loader", tslint: { configFile: "tslint.json" } }
    ],
    loaders: [
      { test: /\.tsx?$/, loader: "awesome-typescript-loader" },
      {
        test: /\.scss$/,
        loaders: [
            'style?sourceMap',
            'css?modules&importLoaders=1&localIdentName=[path]___[name]__[local]___[hash:base64:5]',
            'sass?sourceMap'
        ]
      },{
      test: /\.css$/,
      loader: "style-loader!css-loader"
      }, {
        test: /\.(woff|woff2|ttf|eot)$/,
        loader: 'file-loader'
      },
      {
          test: /\.(jpe?g|png|gif|svg)$/i,
          loaders: [
              'file?hash=sha512&digest=hex&name=[hash].[ext]',
              'image-webpack?bypassOnDebug&optimizationLevel=7&interlaced=false'
          ]
      }
    ],
  },
};
