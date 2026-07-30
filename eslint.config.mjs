import pluginJs from '@eslint/js';
import typescriptEslint from '@typescript-eslint/eslint-plugin';
import typescriptParser from '@typescript-eslint/parser';
import vitestPlugin from '@vitest/eslint-plugin';
import prettier from 'eslint-config-prettier';
import pluginCheckFile from 'eslint-plugin-check-file';
import reactHooks from 'eslint-plugin-react-hooks';
import reactRefresh from 'eslint-plugin-react-refresh';
import globals from 'globals';

export default [
  {
    // General Ignore Patterns
    ignores: ['**/dist/*', '**/node_modules/*'],
  },
  pluginJs.configs.recommended,
  {
    files: ['{services,packages}/**/*.{js,jsx,ts,tsx}'],
    languageOptions: {
      // Base Configuration for JS/TS Files
      parser: typescriptParser,
      ecmaVersion: 'latest',
      sourceType: 'module',
      globals: {
        // Node-Specific Globals
        ...globals.node,
        // Browser-Specific Globals
        ...globals.browser,
        ...vitestPlugin.environments.env.globals,
      },
    },
    plugins: {
      '@typescript-eslint': typescriptEslint,
      'check-file': pluginCheckFile,
      vitest: vitestPlugin,
      // React-Specific Plugins
      'react-refresh': reactRefresh,
      'react-hooks': reactHooks,
    },
    rules: {
      'check-file/filename-naming-convention': [
        'error',
        { '**/*.{js,ts,jsx,tsx}': 'CAMEL_CASE' },
        { ignoreMiddleExtensions: true },
      ],
      ...typescriptEslint.configs.recommended.rules,
      'no-unused-vars': 'off',
      '@typescript-eslint/no-unused-vars': ['error'],
      // React-Specific Rules
      'react-refresh/only-export-components': 'warn',
      // vitest Rules
      ...vitestPlugin.configs.recommended.rules,
    },
  },
  // Integrate Prettier for Formatting
  prettier,
];
