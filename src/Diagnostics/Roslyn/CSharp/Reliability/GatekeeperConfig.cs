// ==++==
// 
//   Copyright (c) Microsoft Corporation.  All rights reserved.
// 
// ==--==
using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;

namespace Microsoft.Gatekeeper
{
    // Copied from ProjectN Gatekeeper sources.
   
    //
    // Central class for reading the Xml config file and applying any policy to override the
    // behavior of the tool
    //
    public class GatekeeperConfig
    {
        HashSet<string> unsupportedTypes = new HashSet<string>();
        HashSet<string> unsupportedMethods = new HashSet<string>();
        HashSet<Tuple<string, string>> unsupportedContracts = new HashSet<Tuple<string, string>>();
        private string configFilePath;

        private GatekeeperConfig(string configFile)
        {
            configFilePath = Path.GetFullPath(configFile);
        }
        
        public HashSet<string> UnsupportedTypes
        {
            get
            {
                return unsupportedTypes;
            }
        }

        public HashSet<string> UnsupportedMethods
        {
            get
            {
                return unsupportedMethods;
            }
        }

        public HashSet<Tuple<string, string>> UnsupportedContracts
        {
            get
            {
                return unsupportedContracts;
            }
        }

        // Should take something from AdditionalDocuments rather than opening the file here.
        public static GatekeeperConfig CreateFromFile(string fileName)
        {
            if (!File.Exists(fileName))
            {
                throw new FileNotFoundException(fileName);
            }

            var newConfig = new GatekeeperConfig(fileName);

            XmlDocument doc = new XmlDocument();
            XmlReaderSettings xmlReaderSettings = new XmlReaderSettings();
            // FXCop compliance (CA3053) requires either no Xml resolver or a secure one.  We don't need one at all so leave it null.
            xmlReaderSettings.XmlResolver = null;
            doc.XmlResolver = null;
            using (XmlReader xmlReader = XmlReader.Create(fileName, xmlReaderSettings))
            {
                doc.Load(xmlReader);
                
                var rootElement = doc.DocumentElement;

                //
                // Unsupported types
                //
                XmlNode types = (XmlElement)rootElement.SelectSingleNode("/config/unsupported/types");
                foreach (var child in types.ChildNodes)
                {
                    XmlElement type = child as XmlElement;
                    if (type == null)
                        continue;

                    if (!type.Name.Equals("type", StringComparison.OrdinalIgnoreCase))
                    {
                        throw new XmlException("Malformed type element");
                    }
                    newConfig.unsupportedTypes.Add(type.InnerText);
                }

                //
                // Unsupported methods
                //
                XmlNode methods = (XmlElement)rootElement.SelectSingleNode("/config/unsupported/methods");
                foreach (var child in methods.ChildNodes)
                {
                    XmlElement method = child as XmlElement;
                    if (method == null)
                        continue;

                    if (!method.Name.Equals("method", StringComparison.OrdinalIgnoreCase))
                    {
                        throw new XmlException("Malformed method element");
                    }
                    newConfig.unsupportedMethods.Add(method.InnerText);
                }

                //
                // Unsupported contracts
                //
                XmlNode contracts = (XmlElement)rootElement.SelectSingleNode("/config/unsupported/contracts");
                foreach (var node in contracts.ChildNodes)
                {
                    XmlElement contract = node as XmlElement;
                    if (contract == null)
                    {
                        continue;
                    }
                    if (!contract.Name.Equals("contract", StringComparison.OrdinalIgnoreCase))
                    {
                        throw new XmlException("Malformed contract element");
                    }

                    string name = contract.GetElementsByTagName("name")[0].InnerText;
                    string publicKeyToken = contract.GetElementsByTagName("publicKeyToken")[0].InnerText;

                    newConfig.unsupportedContracts.Add(new Tuple<string, string>(name, publicKeyToken));
                }
            }

            return newConfig;
        }
    }
}
